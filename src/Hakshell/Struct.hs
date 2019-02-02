-- | Hakshell tries to avoid passing all information around as flat strings. Instead a JSON-like
-- structured data type is used wherever possible.
module Hakshell.Struct
  ( -- * The Struct Data Type
    StructIndex(..), Struct(..),
    -- * The Class of 'Structured' Data Types
    Structured(..), LispExpression(..), fromStruct, objString, objAtom,
    objList, objSizedList, objAccumDict,
    -- * The 'Atom' data type
    Atom, toAtom, atomToText, isAtomChar,
    -- * Haskell Data to/from a 'Struct'
    ParseStruct, runParseStruct, 
    StructParserError(..),
    Control.Monad.Except.throwError,
    Control.Monad.Except.catchError,
    -- * Lisp Expressions
    FromLispExpression(..), forceFromLispExpr, lispYieldedValue,
  ) where

import           Prelude              hiding (fail)

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Except hiding (fail)
import           Control.Monad.Fail
import           Control.Monad.State  hiding (fail)
import           Control.Monad.ST

import           Data.Bits
import           Data.Char
import           Data.List                   (partition)
import qualified Data.Map                    as Map
import           Data.Semigroup
import qualified Data.ByteString.Char8       as Strict
import           Data.Typeable
import qualified Data.Vector                 as Vec
import qualified Data.Vector.Unboxed         as UVec
import qualified Data.Vector.Mutable         as MVec
import qualified Data.Vector.Unboxed.Mutable as UMVec
import           Data.Word

----------------------------------------------------------------------------------------------------

-- | An atom is essentially a string that does not need to be quoted in the context of a Lisp
-- expression.
newtype Atom = Atom{ atomToText :: Strict.ByteString }
  deriving (Eq, Ord)

instance Show Atom where { show (Atom a) = Strict.unpack a; }
instance Read Atom where
  readsPrec _ str = case span isAtomChar str of
    ("", _ ) -> []
    (a, str) -> [(Atom $ Strict.pack a, str)]

instance Semigroup Atom where { (Atom a) <> (Atom b) = Atom (a <> b); }

-- | Takes as many characters from the 'Prelude.head' of the given 'Prelude.String' that match the
-- 'isAtomChar' predicate. 'Atom's may not be null strings, so if there are no characters in the
-- string which satisfy 'isAtomChar', the result returned is 'Prelude.Nothing'.
toAtom :: String -> Maybe Atom
toAtom = takeWhile isAtomChar >>> \ a ->
  if null a then Nothing else Just $ Atom $ Strict.pack a

-- | This predicate matches 'isAlphaNum', along with @'_'@ (underscore), @'.'@ (dot), @':'@ (colon),
-- @'/'@ (slash), @'-'@ (minus or hyphen), @'+'@ (plus), @'*'@ (asterisk), @'\@'@ (at sign)
isAtomChar :: Char -> Bool
isAtomChar = (`divMod` _cellSize) . ord >>> \ (i, b) ->
  maybe False (`testBit` b) (_charTable UVec.!? i)

type BitMapCell = Word32

_initWord :: BitMapCell
_initWord = 0

_cellSize :: Int
_cellSize = finiteBitSize _initWord

_atomChars :: [Int]
_atomChars = ord <$> ("+-*/._:@" ++ ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z'])

_maxIdx :: Int
_maxIdx = maximum _atomChars

_charTable :: UVec.Vector BitMapCell
_charTable = UVec.create
  (do vec <- UMVec.replicate (_maxIdx + 1) _initWord
      forM_ _atomChars $ (`divMod` _cellSize) >>> \ (i, b) ->
        UMVec.read vec i >>= UMVec.write vec i . (.|. (bit b))
      return vec
  )

----------------------------------------------------------------------------------------------------

-- | A path that points to an element in a 'Struct'.
--
-- For those familiary with JavaScript or JSON, an object assigned to a variable @obj@ may be
-- accessed with square-bracket notation like so: @obj[0]["dict key"][1]...@ and so on, this data
-- structure behaves as the sequence of square-bracketed indicies.
data StructIndex
  = StructLeaf
  | StructIndexList Int               StructIndex
  | StructIndexDict Strict.ByteString StructIndex
  deriving (Eq, Typeable)

-- | A simple data type for structured data, which can be both parsed and printed to/from JSON,
-- Lisp-like S-Expressions, or XML.
data Struct
  = ObjInt    !Int
  | ObjChar   !Char
  | ObjFloat  !Double
  | ObjString !Strict.ByteString
  | ObjAtom   !Strict.ByteString
    -- ^ same as 'ObjString' but is known to contain no whitespace in the string
  | ObjList   !(Vec.Vector Struct)
  | ObjDict   !(Map.Map Strict.ByteString Struct)
  deriving (Eq, Ord)

-- | The class of data types that can be converted to and from a 'Struct'.
class Structured a where
  toStruct    :: a -> Struct
  parseStruct :: Struct -> ParseStruct a

instance Structured Struct where { toStruct = id; parseStruct = return; }

instance Structured Int where
  toStruct    = ObjInt
  parseStruct = \ case { ObjInt a -> return a; _ -> mzero; }

instance Structured Char where
  toStruct    = ObjChar
  parseStruct = \ case { ObjChar a -> return a; _ -> mzero; }

instance Structured Double where
  toStruct    = ObjFloat
  parseStruct = \ case { ObjFloat a -> return a; _ -> mzero; }

instance Structured String where
  toStruct    = ObjString . Strict.pack
  parseStruct = \ case { ObjString a -> return (Strict.unpack a); _ -> mzero; }

instance Structured Strict.ByteString where
  toStruct    = ObjString
  parseStruct = \ case { ObjString a -> return a; _ -> mzero; }

instance Structured a => Structured (Vec.Vector a) where
  toStruct    = ObjList . fmap toStruct
  parseStruct = \ case
    ObjList a -> objSizedVector (Vec.length a) <$> mapM parseStruct (Vec.toList a)
    _         -> mzero

instance Structured a => Structured (Map.Map Strict.ByteString a) where
  toStruct    = ObjDict . fmap toStruct
  parseStruct = \ case
    ObjDict a -> Map.fromList <$> forM (Map.assocs a) (\ (key, val) -> (,) key <$> parseStruct val)
    _         -> mzero

instance Structured StructIndex where
  toStruct =
    let loop stack item size = seq size $! case item of
          StructIndexList int more -> loop (ObjInt    int : stack) more $! size + 1
          StructIndexDict str more -> loop (ObjString str : stack) more $! size + 1
          StructLeaf               ->
            flip objSizedList (objAtom "index" : stack) $! negate (size + 1)
    in  flip (loop []) 0
  parseStruct = \ case
    ObjList list -> case Vec.toList list of
      a:ax | a == objAtom "index" -> do
        let loop = \ case
              []               -> pure StructLeaf
              (ObjInt    i):ax -> StructIndexList i <$> loop ax
              (ObjString i):ax -> StructIndexDict i <$> loop ax
              _                -> fail "index contains non-int/string element"
        loop ax
      _                           -> mzero
    _                           -> mzero

-- | A synonym for the 'ObjString' constructor, provided for consistent naming with the other
-- function names starting with @obj...@.
objString :: Strict.ByteString -> Struct
objString = ObjString

-- | Takes as many alpha-numeric characters as it can to construct a string. Any character matching
-- the 'isAlphaNum' predicate can be included. All characters beyond the first non-alpha-numeric are
-- dropped.
objAtom :: Strict.ByteString -> Struct
objAtom = ObjAtom . Strict.takeWhile isAtomChar

-- | A synonym for the 'ObjList' constructor, provided for consisteng naming with the other
-- function names starting with @obj...@.
objList :: Structured a => [a] -> Struct
objList = ObjList . Vec.fromList . fmap toStruct

-- | Construct a list of a known length. If the given size is negative, the list is stored in
-- reverse order.
objSizedList :: Structured a => Int -> [a] -> Struct
objSizedList size = ObjList . objSizedVector size . fmap toStruct

-- | A function such as this ought to be included in the "Vec.Vector" module, but I don't think it
-- is at the time of this writing.
objSizedVector :: forall a . Int -> [a] -> Vec.Vector a
objSizedVector size elems = Vec.create list where
  indicies = if size >= 0 then [0 .. size - 1] else negate <$> [size + 1, size + 2 .. 0]
  list :: ST s (MVec.STVector s a)
  list = do
    mvec <- MVec.new (abs size)
    mapM_ (uncurry (MVec.write mvec)) $ zip indicies elems
    return mvec

-- | Construct a dictionary from a list of key-value pairs. The values in each pair should be a
-- list. If the list is empty, it is not included in the dictionary, if the dictionary contains only
-- one element, the element is stored as the value alone, if the dictionary contains more than one
-- element, the elements are stored into a list.
objAccumDict :: Structured a => [(Strict.ByteString, [a])] -> Struct
objAccumDict = ObjDict . fmap (\ case { [a] -> a; lst -> objList lst; }) .
  Map.fromListWith (++) . fmap (fmap (fmap toStruct)) . filter (not . null . snd)

----------------------------------------------------------------------------------------------------

newtype ParseStruct a = ParseStruct (ExceptT StructParserError (State StructParserState) a)
  deriving (Functor, Applicative, Monad)

instance Alternative ParseStruct where
  empty = ParseStruct $ throwError StructParserBacktrack
  (ParseStruct a) <|> (ParseStruct b) = ParseStruct $ catchError a $ \ case
    StructParserBacktrack -> b
    a                     -> throwError a

instance MonadPlus ParseStruct where { mzero = empty; mplus = (<|>); }

instance MonadState StructParserState ParseStruct where { state = ParseStruct . state; }

instance MonadError StructParserError ParseStruct where
  throwError = ParseStruct . throwError
  catchError (ParseStruct try) catch = ParseStruct $
    catchError try $ (\ (ParseStruct o) -> o) . catch

instance MonadFail ParseStruct where
  fail msg = get >>= \ st -> throwError StructParserError
    { structParserErrorInfo = Strict.pack msg
    , structParserErrorPath = theStructParserPath st
    }

instance Semigroup a => Semigroup (ParseStruct a) where { a <> b = (<>) <$> a <*> b; }

data StructParserError
  = StructParserBacktrack
  | StructParserError
    { structParserErrorInfo :: !Strict.ByteString
    , structParserErrorPath :: !StructIndex
    }

_key_backtracked :: Strict.ByteString
_key_backtracked = "backtracked"

_key_error :: Strict.ByteString
_key_error = "error"

instance Structured StructParserError where
  toStruct = \ case
    StructParserBacktrack     -> objList [objAtom _key_backtracked]
    StructParserError txt idx -> objList [objAtom _key_error, objString txt, toStruct idx]
  parseStruct struct = do
    lst <- parseStruct struct
    key <- parseStruct $ lst Vec.! 0
    if key == _key_backtracked then guard (Vec.length lst == 1) >> return StructParserBacktrack else
      if key == _key_error
       then guard (Vec.length lst == 3) >>
        StructParserError <$> parseStruct (lst Vec.! 1) <*> parseStruct (lst Vec.! 2)
       else empty

instance LispExpression StructParserError where
  toLispExpr   = structuredToLisp
  fromLispExpr = structuredFromLisp

structuredToLisp :: Structured a => a -> String -> String
structuredToLisp = toLispExpr . toStruct

structuredFromLisp :: Structured a => Int -> String -> [(FromLispExpression a, String)]
structuredFromLisp p = fromLispExpr p >=> loop where
  loop (lisp, rem) = case lisp of
    LispExpressionSyntax err -> [(LispExpressionSyntax err, rem)]
    LispExpressionBad    err -> [(LispExpressionBad    err, rem)]
    LispAwaitingInput   cont -> [(LispAwaitingInput $ cont >=> loop, rem)]
    LispYieldedValue  struct -> case fromStruct struct of
      Left err -> [(LispExpressionBad err, rem)]
      Right  a -> [(LispYieldedValue  a  , rem)]

instance Show StructParserError where { showsPrec _ = toLispExpr . toStruct; }
instance Read StructParserError where { readsPrec _ = error "TODO"; }

data StructParserState
  = StructParserState
    { theStructParserDepth :: !Int
    , theStructParserPath  :: !StructIndex
    }

initStructParserState :: StructParserState
initStructParserState = StructParserState
  { theStructParserDepth = 0
  , theStructParserPath  = StructLeaf
  }

runParseStruct :: ParseStruct a -> Either StructParserError a
runParseStruct (ParseStruct f) = evalState (runExceptT f) initStructParserState

fromStruct :: Structured a => Struct -> Either StructParserError a
fromStruct = runParseStruct . parseStruct

----------------------------------------------------------------------------------------------------

data FromLispExpression a
  = LispYieldedValue      a
  | LispAwaitingInput    (String -> [(FromLispExpression a, String)])
  | LispExpressionSyntax !Strict.ByteString
    -- ^ Thrown when the string is invalid Lisp syntax.
  | LispExpressionBad    !StructParserError
    -- ^ Thrown when the string is valid syntax but cannot be constructed by 'fromStruct'.

lispYieldedValue :: FromLispExpression a -> Bool
lispYieldedValue = \ case { LispYieldedValue{} -> True; _ -> False; }

forceFromLispExpr :: [(FromLispExpression a, String)] -> [(a, String)]
forceFromLispExpr = uncurry (++) . partition (lispYieldedValue . fst) >>> \ case
  (a, rem) : _ -> case a of
    LispYieldedValue     a   -> [(a, rem)]
    LispAwaitingInput{}      -> waiting rem
    LispExpressionSyntax err -> error $ Strict.unpack err
    LispExpressionBad    err -> error $ show err
  []           -> []
  where
    waiting rem = error $ "Waiting for more input ("++show (take 20 rem)++")"

charTo3CharCode :: Vec.Vector Strict.ByteString
charTo3CharCode = Vec.fromList $
  [ "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "a"
  , "b"  , "t"  , "n"  , "v"  , "f"  , "r"  , "SO" , "SI"
  , "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB"
  , "CAN", "EM" , "SUB", "ESC", "FS" , "GS" , "RS" , "US"
  , "space"
  ]

charFrom3CharCode :: Map.Map Strict.ByteString Char
charFrom3CharCode =
  Map.insert "space"    ' ' .
  Map.insert "newline" '\n' .
  Map.insert "tab"     '\t' $
  Map.fromList (Vec.toList charTo3CharCode `zip` (chr <$> [0 ..]))

-- | The class of data types that can be converted to and from a Lisp-like "S-Expression".
class LispExpression a where
  toLispExpr   :: a -> String -> String
  fromLispExpr :: Int -> String -> [(FromLispExpression a, String)]

instance LispExpression Int         where
  toLispExpr   = shows
  fromLispExpr = error "TODO -- fromLispExpr :: String -> (Int)"

instance LispExpression Char        where
  toLispExpr c = ("#\\" ++) . maybe (c :) ((++) . Strict.unpack) (charTo3CharCode Vec.!? ord c)
  fromLispExpr = error "TODO -- fromLispExpr :: Char -> (Char)"

instance LispExpression Double      where
  toLispExpr   = shows
  fromLispExpr = error "TODO -- fromLispExpr :: String -> (Double)"

instance LispExpression String      where
  toLispExpr   = shows
  fromLispExpr = error "TODO -- fromLispExpr :: String -> (String)"

instance LispExpression Strict.ByteString where
  toLispExpr   = shows
  fromLispExpr = error "TODO -- fromLispExpr :: String -> (Strict.ByteString)"

instance LispExpression Atom        where
  toLispExpr   = shows
  fromLispExpr = error "TODO -- fromLispExpr :: String -> (Atom)"

instance Structured a => LispExpression [a] where
  toLispExpr   = fmap toStruct >>> \ case
    []         -> ("()" ++)
    a:ax       -> let cons = (:) in cons '(' . toLispExpr a .
      let loop ax = case ax of { [] -> cons ')'; a:ax -> toLispExpr a . loop ax; } in loop ax
  fromLispExpr = error "TODO -- fromLispExpr :: String -> (Struct)"

instance Structured a => LispExpression (Vec.Vector a) where
  toLispExpr   = Vec.toList >>> toLispExpr
  fromLispExpr = error "TODO -- fromLispExpr :: String -> (Struct)"

instance Structured a => LispExpression (Map.Map Strict.ByteString a) where
  toLispExpr   = let cons = (:) in Map.assocs . fmap toStruct >>> \ case
    []   -> ("{}" ++)
    a:ax -> 
      let pair (key, val) = cons colon . toLispExpr key . cons ' ' . toLispExpr val . loop ax
          loop = \ case{ [] -> cons '}'; a:ax -> pair a . loop ax; }
      in  cons '{' . pair a . cons ' ' . loop ax
  fromLispExpr = error "TODO -- Structured a => fromLispExpr :: String -> (Map.Map Strict.ByteString a)"

colon :: Char
colon = ':'

instance LispExpression Struct      where
  toLispExpr = \ case
    ObjInt    o -> toLispExpr o
    ObjChar   o -> toLispExpr o
    ObjFloat  o -> toLispExpr o
    ObjString o -> toLispExpr o
    ObjAtom   o -> toLispExpr o
    ObjList   o -> toLispExpr o
    ObjDict   o -> toLispExpr o
  fromLispExpr = error "TODO -- fromLispExpr :: String -> (Struct)"
