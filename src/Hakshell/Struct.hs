-- | Hakshell tries to avoid passing all information around as flat strings. Instead a structured
-- data type 'Struct' is used wherever possible. 'Struct' approximates the Abstract Syntax Tree
-- (AST) of a Lisp expression (specificially the Scheme dialect of Lisp), and in fact a 'Struct' can
-- be converted to and parsed from Lisp-like code.
module Hakshell.Struct
  ( -- * The Struct Data Type
    Struct(..), StructIndex(..), structIndex,
    -- * The Class of 'Structured' Data Types
    Structured(..), LispPrim, toLispPrim, fromLispPrim,
    strToLispExpr, strFromLispExpr, fromStruct, objInt, objString, objAtom,
    objList, objSizedList,
    -- ** Dictionary structures
    Dictionary,
    -- * The 'Atom' data type
    Atom, toAtom, atomToText, isAtomChar, atomHead,
    -- * Haskell Data to/from a 'Struct'
    ParseStruct, runParseStruct,
    StructParseError(..), NonUniqueKeyError(..),
    annotateParse, structRequire,
    Control.Monad.Except.throwError,
    Control.Monad.Except.catchError,
    -- * Haskell Data to/from a list of 'Struct's
    ParseListStruct, parseList, parseListWith,
    getStructLength, getStructIndex, putStructIndex, getStructVector, putStructVector,
    takeList, takeStruct, takeElem, scanElems,
    takeAtom, takeBool, takeInt, takeFloat, takeString, takeEndOfList,
    -- ** Slices of Lists
    ListSlice, takeSlice, takeSliceToEnd,
    -- * Dictionaries
    ParseDictStruct, parseDictionary, takeKey, takeUnique, takeKeyAll,
    -- ** Data structures
    parseData, takeKeyAtom, scanUntilKeyAtom, listToDictionary,
    -- * Lisp Expressions
    FromLispPrim(..), forceFromLispExpr, lispYieldedValue,
  ) where

import           Prelude              hiding (fail)

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Monad.Except hiding (fail)
import           Control.Monad.Fail
import           Control.Monad.State  hiding (fail)
import           Control.Monad.ST

import           Data.Bits
import           Data.Char
import           Data.List                   (partition)
import qualified Data.List.NonEmpty          as NE
import qualified Data.Map                    as Map
import qualified Data.ByteString.Char8       as Strict
import qualified Data.ByteString.UTF8        as UTF8
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
    (a, str) -> [(Atom $ UTF8.fromString a, str)]

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

-- | Get the first character of an 'Atom'.
atomHead :: Atom -> Char
atomHead (Atom str) = maybe '\0' fst $ UTF8.decode str

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
data StructIndexElem = StructIndexList Int | StructIndexDict Atom
  deriving (Eq, Ord)

newtype StructIndex = StructIndex (Vec.Vector StructIndexElem)
  deriving (Eq, Ord)

instance Semigroup StructIndex where { (StructIndex a) <> (StructIndex b) = StructIndex (a <> b); }
instance Monoid    StructIndex where { mappend = (<>); mempty = StructIndex Vec.empty; }

-- | Construct a 'StructIndex' value, you must provide the length of the path. The given list is
-- reversed before being entered into the vector.
structIndex :: Int -> [StructIndexElem] -> StructIndex
structIndex n path = StructIndex $ Vec.create
  (do vec <- MVec.new n
      mapM_ (uncurry $ MVec.write vec) $ flip zip path $
        takeWhile (>= 0) $ iterate (subtract 1) (n - 1)
      return vec
  )

structPathFromParseState :: ParseStructState -> StructIndex
structPathFromParseState st = structIndex (theStructParserDepth st) (theStructParserPath st)

nullStructIndex :: StructIndex -> Bool
nullStructIndex (StructIndex idx) = Vec.null idx

-- | A simple data type for structured data, which can be both parsed and printed to/from JSON,
-- Lisp-like S-Expressions, or XML.
data Struct
  = ObjFalse
  | ObjTrue
  | ObjInt    !Integer
  | ObjChar   !Char
  | ObjFloat  !Double
  | ObjString !Strict.ByteString
  | ObjAtom   !Atom
    -- ^ same as 'ObjString' but is known to contain no whitespace in the string
  | ObjList   !(Vec.Vector Struct)
  | ObjQuoted !Struct
  | ObjHashed !Struct
  deriving (Eq, Ord)

-- | The class of data types that can be converted to and from a 'Struct' (a Lisp AST).
class Structured a where
  toStruct    :: a -> Struct
  parseStruct :: Struct -> ParseStruct a

instance Structured Struct where { toStruct = id; parseStruct = return; }

instance Structured Bool   where
  toStruct true = if true then ObjTrue else ObjFalse
  parseStruct = \ case
    ObjFalse -> return False
    ObjTrue  -> return True
    _        -> mzero

instance Structured Int where
  toStruct = ObjInt . fromIntegral
  parseStruct = \ case
    ObjInt a | a <= fromIntegral (maxBound :: Int) -> return $ fromInteger a
    _ -> mzero

instance Structured Integer where
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
  parseStruct = \ case { ObjString a -> return (UTF8.toString a); _ -> mzero; }

instance Structured Strict.ByteString where
  toStruct    = ObjString
  parseStruct = \ case { ObjString a -> return a; _ -> mzero; }

instance Structured Atom where
  toStruct = ObjAtom
  parseStruct = \ case { ObjAtom a -> return a; _ -> mzero; }

instance Structured a => Structured (Vec.Vector a) where
  toStruct    = ObjList . fmap toStruct
  parseStruct = \ case
    ObjList a -> objSizedVector (Vec.length a) <$> mapM parseStruct (Vec.toList a)
    _         -> mzero

instance Structured Dictionary where
  toStruct (Dictionary map) = ObjList $ Vec.fromList $ do
    (key, v NE.:| vx) <- Map.assocs map
    (ObjAtom key : (ObjList . theListSliceVector <$> (v : vx)))
  parseStruct = parseList listToDictionary

instance Structured StructIndexElem where
  toStruct = \ case
    StructIndexList i -> ObjInt (toInteger i)
    StructIndexDict i -> ObjAtom i
  parseStruct = \ case
    ObjInt  o -> pure $ StructIndexList (fromIntegral o)
    ObjAtom o -> pure $ StructIndexDict o
    _         -> mzero

instance Structured StructIndex where
  toStruct (StructIndex vec) = ObjList $ toStruct <$> vec
  parseStruct = parseList $ do
    takeAtom $ guard . (== _lbl_index)
    len <- getStructLength
    fmap (StructIndex . Vec.fromListN (len - 1) . ($ [])) $ scanElems id $ \ fifo i ->
      fmap (\ elem -> (Just (i + 1), fifo . (elem :))) .
      annotateParse "index" .
      structRequire "int or string value" parseStruct

_lbl_index :: Atom
_lbl_index = read "index"

-- | Takes any value in the 'Integral' typeclass and converts it to a 'Struct' using the 'ObjInt'
-- constructor.
objInt :: Integral i => i -> Struct
objInt = ObjInt . fromIntegral

-- | A synonym for the 'ObjString' constructor, provided for consistent naming with the other
-- function names starting with @obj...@.
objString :: Strict.ByteString -> Struct
objString = ObjString

-- | Takes as many alpha-numeric characters as it can to construct a string. Any character matching
-- the 'isAlphaNum' predicate can be included. All characters beyond the first non-alpha-numeric are
-- dropped.
objAtom :: Strict.ByteString -> Struct
objAtom = ObjAtom . Atom . Strict.takeWhile isAtomChar

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

----------------------------------------------------------------------------------------------------

-- not for export
class (Monad m, MonadPlus m, MonadError StructParseError m) => StructParser m where
  getParseState :: m ParseStructState
  putParseState :: ParseStructState -> m ()
  modifyParseState :: (ParseStructState -> ParseStructState) -> m ()

data StructParseError
  = StructParseBacktrack
  | StructParseError
    { structParseErrorType      :: !StructParseErrorType
      -- ^ Set by the 'annotateParse' function.
    , structParseErrorInfo      :: !StructParseErrorInfo
      -- ^ This field is set when the 'fail' function is used to throw the error. It is also set by
      -- 'structRequire' with a generic error message.
    , structParseErrorValue     :: Maybe Struct
      -- ^ This field is set by 'structRequire' with the actual value that did not meet the
      -- requirements, and will set to 'Nothing' in the case that the 'getStructIndex' has moved
      -- beyond the final element in the list.
    , structParseErrorPath      :: !StructIndex
      -- ^ This field is set automatically by all error throwing functions, including 'throwError',
      -- 'fail', and 'structRequire'.
    , structParseErrorNonUnique :: Maybe NonUniqueKeyError
      -- ^ Set by the 'takeUnique' function.
    }

data NonUniqueKeyError
  = NonUniqueKeyError
    { theOffendingKeyName      :: !Atom
    , theOffendingKeyInstances :: [ListSlice]
    }

type StructParseErrorType = Strict.ByteString
type StructParseErrorInfo = Strict.ByteString

instance Structured StructParseError where
  toStruct = \ case
    StructParseBacktrack    -> objList [ObjAtom _key_backtracked]
    StructParseError
      {structParseErrorType=typ
      ,structParseErrorInfo=info
      ,structParseErrorValue=badval
      ,structParseErrorPath=path
      ,structParseErrorNonUnique=nonuniq
      } -> objList $ [ObjAtom _key_error] ++
        (if Strict.null typ  then [] else [ObjAtom _key_error_on_type, objString typ]) ++
        (if Strict.null info then [] else [ObjAtom _key_error_info, objString info]) ++
        maybe [] (\ val -> [ObjAtom _key_error_wrong_value, val]) badval ++
        maybe [] (\ val -> [ObjAtom _non_unique_key, toStruct val]) nonuniq ++
        (if nullStructIndex path then [] else  [ObjAtom _key_error_path, toStruct path])
  parseStruct = parseList $ annotateParse "error-message" $ parseData _key_error $ \ opts -> do
    structRequire "no options" (const $ guard $ null opts) (ObjList opts)
    typ     <- maybe   ""   id <$> optional (takeUnique _key_error_on_type parseStruct)
    info    <- maybe   ""   id <$> optional (takeUnique _key_error_info    parseStruct)
    path    <- takeUnique _key_error_wrong_value parseStruct
    badval  <- optional $ takeUnique _key_error_path parseStruct
    nonuniq <- optional $ takeUnique _non_unique_key parseStruct
    return StructParseError
      { structParseErrorType      = typ
      , structParseErrorInfo      = info
      , structParseErrorValue     = badval
      , structParseErrorPath      = path
      , structParseErrorNonUnique = nonuniq
      }

instance Structured NonUniqueKeyError where
  toStruct (NonUniqueKeyError{theOffendingKeyName=key,theOffendingKeyInstances=slices}) =
    ObjList $ Vec.fromList $ ObjAtom _non_unique_key : ObjAtom key : (toStruct <$> slices)
  parseStruct = parseList $ annotateParse "non-unique-key-error" $ do
    takeAtom $ guard . (_non_unique_key ==)
    offkey  <- takeAtom pure
    instncs <- many $ takeElem pure
    takeEndOfList
    return NonUniqueKeyError
      { theOffendingKeyName      = offkey
      , theOffendingKeyInstances = instncs
      }

instance LispPrim StructParseError where
  toLispPrim   = structuredToLisp
  fromLispPrim = structuredFromLisp

instance Show StructParseError where { showsPrec _ = toLispPrim . toStruct; }
instance Read StructParseError where
  readsPrec p = fromLispPrim p >=> \ (a, str) -> case a of
    LispPrimSyntax err -> fail (UTF8.toString err)
    LispPrimBad      _ -> fail "Failed to parse StructParseError"
    LispWaitInput{}    -> fail "Parser for 'StructParseError' failed, input string incomplete"
    LispYield        a -> [(a, str)]

_non_unique_key :: Atom
_non_unique_key = read "non-unique-key"

parseStepIn :: StructParser m => StructIndexElem -> m a -> m a
parseStepIn idx f = do
  modifyParseState $ (structParserDepth +~ 1) . (structParserPath  %~ (idx :))
  let restore = modifyParseState $ (structParserDepth -~ 1) . (structParserPath %~ tail)
  catchError (f <* restore) $ (restore >>) . throwError

-- | Store some information about the type of parse being run into the parser state, so if an error
-- occurs, this information can be written into a record of the error message.
annotateParse :: StructParser m => StructParseErrorType -> m a -> m a
annotateParse typ f = do
  modifyParseState $ structParserType %~ (typ :)
  let restore = modifyParseState $ structParserType %~ tail
  catchError (f <* restore) $ (restore >>) . throwError

-- not for export
parseThrowError
  :: StructParser m
  => Maybe Struct -> Maybe NonUniqueKeyError -> StructParseErrorInfo -> m void
parseThrowError badvalue nonunique info = do
  st <- getParseState
  throwError $ StructParseError
    { structParseErrorType      = case st ^. structParserType of { [] -> ""; a:_ -> a; }
    , structParseErrorInfo      = info
    , structParseErrorPath      = structPathFromParseState st
    , structParseErrorValue     = badvalue
    , structParseErrorNonUnique = nonunique
    }

----------------------------------------------------------------------------------------------------

-- Helpers for defining state monad transformers which do not instantiate MonadState. Basically all
-- three of the struct parsing types, 'ParseStruct', 'ParseListStruct', and 'ParseDictStruct', all
-- behave the in the exact same way for the instances of 'fail', 'throwError', 'catchError',
-- 'mzero', and 'mplus', but they behave in ways that cannot be derived. So I've defined a set of
-- types and functions for copying this behavior into all of the instances without having to
-- actually copy and paste code.

data ParseStructState
  = ParseStructState
    { theStructParserDepth :: !Int
    , theStructParserPath  :: [StructIndexElem]
    , theStructParserType  :: [Strict.ByteString]
    }

type ParserWrapper st outer a f =
  (ExceptT StructParseError (State st) a -> outer a) ->
  (outer a -> ExceptT StructParseError (State st) a) -> f

wrappedThrowError :: ParserWrapper st outer void (StructParseError -> outer void)
wrappedThrowError wrap _unwrap = wrap . throwError

wrappedCatchError
  :: ParserWrapper st outer a (outer a -> (StructParseError -> outer a) -> outer a)
wrappedCatchError wrap unwrap try catch = wrap $ lift get >>= \ st -> 
  catchError (unwrap try) $ ((lift $ put st) >>) . unwrap . catch

wrappedMPlus :: ParserWrapper st outer a (outer a -> outer a -> outer a)
wrappedMPlus wrap unwrap a b = wrap $ catchError (unwrap a) $ \ case
  StructParseBacktrack -> unwrap b
  err                  -> throwError err

wrappedMZero :: ParserWrapper st outer void (outer void)
wrappedMZero wrap _unwrap = wrap $ throwError StructParseBacktrack

wrappedMFail :: StructParser m => String -> m void
wrappedMFail = parseThrowError Nothing Nothing . Strict.pack

----------------------------------------------------------------------------------------------------

newtype ParseStruct a
  = ParseStruct
    { unwrapParseStruct :: ExceptT StructParseError (State ParseStructState) a }
  deriving (Functor, Applicative, Monad)

wrapParseStruct :: ParserWrapper ParseStructState ParseStruct a f -> f
wrapParseStruct w = w ParseStruct unwrapParseStruct

instance Alternative ParseStruct where
  empty = wrapParseStruct wrappedMZero
  (<|>) = wrapParseStruct wrappedMPlus

instance MonadPlus ParseStruct where { mzero = empty; mplus = (<|>); }

instance MonadError StructParseError ParseStruct where
  throwError = wrapParseStruct wrappedThrowError
  catchError = wrapParseStruct wrappedCatchError

instance MonadFail ParseStruct where { fail = wrappedMFail; }

instance Semigroup a => Semigroup (ParseStruct a) where { a <> b = (<>) <$> a <*> b; }

instance StructParser ParseStruct where
  getParseState    = ParseStruct $ lift get
  putParseState    = ParseStruct . lift . put
  modifyParseState = ParseStruct . lift . modify

_key_backtracked :: Atom
_key_backtracked = read "backtracked"

_key_error :: Atom
_key_error = read "error"

_key_error_on_type :: Atom
_key_error_on_type = read ":parsing"

_key_error_info :: Atom
_key_error_info = read ":because"

_key_error_path :: Atom
_key_error_path = read ":at-index"

_key_error_wrong_value :: Atom
_key_error_wrong_value = read ":wrong-value"

_key_error_non_unique :: Atom
_key_error_non_unique = read ":non-unique-key"

structuredToLisp :: Structured a => a -> String -> String
structuredToLisp = toLispPrim . toStruct

structuredFromLisp :: Structured a => Int -> String -> [(FromLispPrim a, String)]
structuredFromLisp p = fromLispPrim p >=> loop where
  loop (lisp, rem) = case lisp of
    LispPrimSyntax err -> [(LispPrimSyntax err, rem)]
    LispPrimBad    err -> [(LispPrimBad    err, rem)]
    LispWaitInput cont -> [(LispWaitInput $ cont >=> loop, rem)]
    LispYield   struct -> case fromStruct struct of
      Left err -> [(LispPrimBad err, rem)]
      Right  a -> [(LispYield   a  , rem)]

structParserDepth :: Lens' ParseStructState Int
structParserDepth = lens theStructParserDepth $ \ a b -> a{ theStructParserDepth = b }

structParserPath :: Lens' ParseStructState [StructIndexElem]
structParserPath = lens theStructParserPath $ \ a b -> a{ theStructParserPath = b }

structParserType :: Lens' ParseStructState [Strict.ByteString]
structParserType = lens theStructParserType $ \ a b -> a{ theStructParserType = b }

initParseStructState :: ParseStructState
initParseStructState = ParseStructState
  { theStructParserDepth = 0
  , theStructParserPath  = []
  , theStructParserType  = []
  }

runParseStruct
  :: MonadError StructParseError m
  => ParseStruct a -> ParseStructState -> m a
runParseStruct (ParseStruct f) = (throwError ||| return) . evalState (runExceptT f)

-- | Evaluate 'parseStruct' on some 'Struct' to produce a value of type @a@.
fromStruct :: Structured a => Struct -> Either StructParseError a
fromStruct = flip runParseStruct initParseStructState . parseStruct

-- | Run a 'ParseStruct' function and throw an exception if it backtracks.
structRequire
  :: StructParser m
  => StructParseErrorInfo -> (Struct -> m a) -> Struct -> m a
structRequire info f o = mplus (f o) $ parseThrowError (Just o) Nothing ("require " <> info)

----------------------------------------------------------------------------------------------------

-- | Like 'ParseStruct' but has combinators designed specifically for inspecting lists.
newtype ParseListStruct a
  = ParseListStruct
    { unwrapParseListStruct :: ExceptT StructParseError (State ParseListStructState) a }
  deriving (Functor, Applicative, Monad)

data ParseListStructState
  = ParseListStructState
    { theListContext :: !ParseStructState
    , theListVector  :: !(Vec.Vector Struct)
    , theListIndex   :: !Int
    }

wrapParseListStruct :: ParserWrapper ParseListStructState ParseListStruct a f -> f
wrapParseListStruct w = w ParseListStruct unwrapParseListStruct

instance Alternative ParseListStruct where
  empty = wrapParseListStruct wrappedMZero
  (<|>) = wrapParseListStruct wrappedMPlus

instance MonadPlus ParseListStruct where { mzero = empty; mplus = (<|>); }

instance MonadError StructParseError ParseListStruct where
  throwError = wrapParseListStruct wrappedThrowError
  catchError = wrapParseListStruct wrappedCatchError

instance MonadFail ParseListStruct where { fail = wrappedMFail; }

instance Semigroup a => Semigroup (ParseListStruct a) where { a <> b = (<>) <$> a <*> b; }

instance StructParser ParseListStruct where
  getParseState    = view listContext <$> parseListGetState
  putParseState    = parseListModifyState . (listContext .~)
  modifyParseState = parseListModifyState . (listContext %~)

parseListGetState    :: ParseListStruct ParseListStructState
parseListGetState    = ParseListStruct $ lift get

parseListModifyState :: (ParseListStructState -> ParseListStructState) -> ParseListStruct ()
parseListModifyState = ParseListStruct . lift . modify

listContext :: Lens' ParseListStructState ParseStructState
listContext = lens theListContext $ \ a b -> a{ theListContext = b }

listIndex :: Lens' ParseListStructState Int
listIndex = lens theListIndex $ \ a b -> a{ theListIndex = b }

listVector :: Lens' ParseListStructState (Vec.Vector Struct)
listVector = lens theListVector $ \ a b -> a{ theListVector = b }

getStructVector :: ParseListStruct (Vec.Vector Struct)
getStructVector = theListVector <$> parseListGetState

putStructVector :: Vec.Vector Struct -> ParseListStruct ()
putStructVector = parseListModifyState . (listVector .~)

getStructIndex :: ParseListStruct Int
getStructIndex = view listIndex <$> parseListGetState

putStructIndex :: Int -> ParseListStruct ()
putStructIndex = parseListModifyState . (listIndex .~)

getStructLength :: ParseListStruct Int
getStructLength = Vec.length <$> getStructVector

-- | Begin parsing over a list 'Struct'. This function immediately evaluates to 'empty' if the given
-- 'Struct' is not a list. Consider this the entry point for the 'ParseListStruct' function type,
-- the 'ParseListStruct' analogue of 'runState'.
parseList :: ParseListStruct a -> Struct -> ParseStruct a
parseList f = \ case
  ObjList vec -> parseListWith vec f
  _           -> empty

-- | Like 'parseList', but more useful in a @case@ statement over 'Struct's because you can evaluate
-- it with the value of an 'ObjList' constructor.
parseListWith :: Vec.Vector Struct -> ParseListStruct a -> ParseStruct a
parseListWith vec (ParseListStruct f) = do
  st <- getParseState
  (result, st) <- pure $ fmap theListContext $ runState (runExceptT f) $ ParseListStructState
    { theListContext = st & structParserDepth +~ 1
    , theListVector  = vec
    , theListIndex   = 0
    }
  putParseState (st & structParserDepth -~ 1)
  (throwError ||| return) result

-- | Take the next element in the list, without regard for it's type.
takeStruct :: (Struct -> ParseStruct a) -> ParseListStruct a
takeStruct f = do
  i   <- getStructIndex
  len <- getStructLength
  vec <- getStructVector
  if not $ 0 <= i && i < len then mzero else parseStepIn (StructIndexList i) $ do
    st <- getParseState
    (runParseStruct $ f $ vec Vec.! i) $
      (st & (structParserDepth +~ 1) . (structParserPath %~ ((StructIndexList i) :)))

-- | Evaluates 'takeStruct' with the 'parseStruct' instance function for the type @elem@, and then
-- evaluates a continuation on that @elem@ value.
takeElem :: Structured elem => (elem -> ParseStruct a) -> ParseListStruct a
takeElem = takeStruct . (parseStruct >=>)

-- | Succeeds if 'getStructIndex' is out of bounds (less then zero or greater than
-- 'getStructLength'), fails if the index is in bounds.
takeEndOfList :: ParseListStruct ()
takeEndOfList = do
  i   <- getStructIndex
  len <- getStructLength
  guard (i >= len || i < 0)

-- | Within a list parser, step into a list element
takeList :: ParseListStruct a ->  ParseListStruct a
takeList = takeStruct . parseList

-- | 'takeElem' specialized for an 'Atom' type.
takeAtom :: (Atom -> ParseStruct a) -> ParseListStruct a
takeAtom = takeElem

-- | 'takeElem' specialized for a 'Bool' type.
takeBool :: (Bool -> ParseStruct a) -> ParseListStruct a
takeBool = takeElem

-- | 'takeElem' specialized for an 'Integer' type.
takeInt :: (Integer -> ParseStruct a) -> ParseListStruct a
takeInt = takeElem

-- | 'takeElem' specialized for a 'Double' type.
takeFloat :: (Double -> ParseStruct a) -> ParseListStruct a
takeFloat = takeElem

-- | 'takeElem' specialized for a string type.
takeString :: (Strict.ByteString -> ParseStruct a) -> ParseListStruct a
takeString = takeElem

-- | Iterate over elements in a list, folding each each element into a value with a given
-- continuation function. The continuation also receives the index of the 'Struct' element. The
-- continuation must return the updated folded value, and may return the next index to scan. If the
-- index returned is out of bounds, or if 'Nothing' is returned instead of an next index to scan,
-- then looping ends and the @fold@ result is returned. If the continuation function backtracks, the
-- entire scan operation backtracks and no @fold@ result is returned.
scanElems
  :: fold -- ^ the value to fold as the scan procedes,
  -> (fold -> Int -> Struct -> ParseStruct (Maybe Int, fold))
    -- ^ the action to perform on each scanned element. Return the updated index and the @fold@
    -- values. If the returned index is out of bounds, the loop halts.
  -> ParseListStruct fold
scanElems fold f = loop fold where
  loop fold = do
    i <- getStructIndex
    (i, fold) <- takeStruct $ f fold i
    case i of
      Nothing -> return fold
      Just  i -> do
        putStructIndex i
        len <- getStructLength
        if 0 <= i || i < len then loop fold else return fold

-- not for export
--
-- Parse over a vector that has been extracted from the current list, restore the state on failure.
parseSubVector
  :: ParseListStruct a
  -> Vec.Vector Struct
  -> ParseListStruct a
parseSubVector (ParseListStruct (ExceptT parse)) subvec = do
  st <- parseListGetState
  case runState parse $ listVector .~ subvec $ listIndex .~ 0 $ st of
    (result, st) -> case result of
      Left  err    -> parseListModifyState (const st) >> throwError err
      Right result -> return result

-- | Take a slice of the list over which this function is parsing, starting from the index @here@
-- given by 'getStructIndex' taking an integer number @many@ elements as the argument given to this
-- function. Like the 
--
-- A slice of the array starting from @here@ and containing @many@ elements is created from the
-- vector that stores the elements of the list, and @many@ may be a negative number in which case
-- elements before @here@ are taken (but the order of elements taken are not reversed in the case of
-- a negative @many@ value). This semantics is similar to the 'Data.List.take' function from
-- "Data.List", although if @many@ is zero this function evaluates to 'empty'.
--
-- Once the slice is selected, it is used to evaluate the given 'ParseListStruct' continuation
-- function. This continuation sees the slice as a new vector, and will observe that the element at
-- the @here@ position is element zero. The depth of the struct parse is not incremented as we are
-- still in the same list of elements as the calling context.
--
-- This function does not modify the index given by 'getStructIndex' unless the given continuation
-- succeeds.
takeSlice :: Int -> ParseListStruct a -> ParseListStruct a
takeSlice len parse = if len == 0 then empty else do
  len  <- getStructLength
  here <- getStructIndex
  let to = here + len
  when (not $ 0 <= here && here < len && 0 <= to && to < len) empty
  let (lo, hi) = (min here to, max here to)
  (Vec.slice lo (hi - lo) <$> getStructVector >>= parseSubVector parse) <* putStructIndex hi

-- | Like 'takeSlice' but takes all elements from the current position to the end of the current
-- list.
takeSliceToEnd :: ParseListStruct a -> ParseListStruct a
takeSliceToEnd parse = do
  len  <- getStructLength
  here <- getStructIndex
  when (not $ 0 <= here && here < len) empty
  (Vec.slice here (len - here) <$> getStructVector >>= parseSubVector parse) <* putStructIndex len

----------------------------------------------------------------------------------------------------

-- | Note that a 'ListSlice' is usually created by the 'listToDictionary' function. However, this
-- data type does instantiate the 'Structured' class and has it's own structured form.
data ListSlice
  = ListSlice
    { theListSliceIndex  :: !Int
      -- ^ The index within the parent list where this slice begins
    , theListSliceVector :: !(Vec.Vector Struct)
    }
  deriving (Eq, Ord)

instance Structured ListSlice where
  toStruct (ListSlice{theListSliceIndex=i,theListSliceVector=vec}) =
    ObjList $ Vec.fromListN (2 + Vec.length vec) $
    [ObjAtom _list_slice, objInt i] ++ Vec.toList vec
  parseStruct = parseList $ do
    takeAtom $ guard . (== _list_slice)
    i <- takeElem pure
    takeSliceToEnd $ do
      vec <- theListVector <$> parseListGetState -- TODO: copy the vector here?
      return ListSlice{ theListSliceIndex = i, theListSliceVector = vec }

-- | The 'ListSlice' is usually used in the context of parsing a 'Dictionary' literal from a list
-- data structure, meaning the slice is usually a number of elements after a dictionary key and
-- before another key. The 'ListSlice' contains a list that is a slice of a larger list, and this
-- slice may contain zero or more elements. Usually, when defining a 'Dictionary' literal, it is
-- expected that there is only 1 element after a key. But the way 'listToDictionary' works, it
-- always provides a 'Vec.Vector' of elements, even when there is only 1 element after a key.
--
-- This function allows you to check the 'Vec.Vector' created by 'listToDictionary', if there is
-- only 1 element after a key, that element is extracted from the vector and returned as it is. If
-- there are 0 elements after a key, 'ObjTrue' is returned (the existence of the key indicates that
-- a flag has been set). If there are more than 1 elements after a key, the elements are returned as
-- a list.
listSliceItem :: ListSlice -> Struct
listSliceItem s = let vec = theListSliceVector s in case Vec.length vec of
  0 -> ObjTrue
  1 -> vec Vec.! 0
  _ -> ObjList vec

_list_slice :: Atom
_list_slice = read "list-slice"

----------------------------------------------------------------------------------------------------

-- | There are a few ways to create a 'Dictionary', but one way is to create one from a bunch of
-- slices of a 'Vec.Vector', where each slice starts with an 'Atom' who's first character is a
-- colon.
newtype Dictionary = Dictionary (Map.Map Atom DictionaryEntry) deriving Eq

type DictionaryEntry = NE.NonEmpty ListSlice

instance Monoid Dictionary where { mempty = Dictionary mempty; mappend = (<>); }

instance Semigroup Dictionary where
  (Dictionary a) <> (Dictionary b) = Dictionary $ Map.unionWith (<>) a b

dictLookup :: Atom -> Dictionary -> Maybe DictionaryEntry
dictLookup a (Dictionary dict) = Map.lookup a dict

dictInsertWith
  :: (DictionaryEntry -> DictionaryEntry -> DictionaryEntry)
  -> Atom -> DictionaryEntry -> Dictionary -> Dictionary
dictInsertWith f a o (Dictionary dict) = Dictionary $ Map.insertWith f a o dict

dictDelete :: Atom -> Dictionary -> Dictionary
dictDelete a (Dictionary map) = Dictionary $ Map.delete a map

-- | Data structures can be represented by an atom followed by an association list. This is a
-- function for parsing data in this form.
parseData
  :: Atom
  -> (Vec.Vector Struct -- ^ this is a list of options after the head and before the first atom
      -> ParseDictStruct a)
  -> ParseListStruct a
parseData atom f = do
  takeAtom $ guard . (== atom)
  scanUntilKeyAtom >>= parseDictionary . f

-- | Takes an atom only if the atom begins with a colon (@':'@) character.
takeKeyAtom :: ParseListStruct Atom
takeKeyAtom = takeAtom $ \ a -> guard (':' == atomHead a) >> return a

-- | Gathers all elements in the list up until a key atom (according to 'takeKeyAtom') is returned.
scanUntilKeyAtom :: ParseListStruct (Vec.Vector Struct)
scanUntilKeyAtom = do
  optsStart <- getStructIndex
  optsLen   <- scanElems 0 $ \ optsLen i -> \ case
    ObjAtom a | atomHead a == ':' -> return (Nothing, optsLen)
    _                             -> return (Just (i + 1), optsLen + 1)
  Vec.slice optsStart optsLen <$> getStructVector

-- | Parse a list structure as a 'Dictionary', with keys deonted as 'Atom's starting with colon
-- characters, and values being anything between keys. The current element must be a key atom,
-- otherwise this function fails. The entire remainder of the list will be consumed after this
-- function is evaluated.
listToDictionary :: ParseListStruct Dictionary
listToDictionary = getKey >>= loop mempty where
  getKey = (,) <$> getStructIndex <*> takeKeyAtom
  loop map (i, key) = do
    elems <- scanUntilKeyAtom
    map   <- pure $ dictInsertWith (flip (<>)) key (ListSlice i elems NE.:| []) map
    mplus (takeEndOfList >> return map) $ getKey >>= loop map

----------------------------------------------------------------------------------------------------

newtype ParseDictStruct a
  = ParseDictStruct
    { unwrapParseDictStruct :: ExceptT StructParseError (State ParseDictStructState) a }
  deriving (Functor, Applicative, Monad)

wrapParseDictStruct :: ParserWrapper ParseDictStructState ParseDictStruct a f -> f
wrapParseDictStruct w = w ParseDictStruct unwrapParseDictStruct

data ParseDictStructState
  = ParseDictStructState
    { theDictContext :: !ParseStructState
    , theDictSource  :: !Dictionary
    }

instance Alternative ParseDictStruct where
  empty = wrapParseDictStruct wrappedMZero
  (<|>) = wrapParseDictStruct wrappedMPlus

instance MonadPlus ParseDictStruct where { mzero = empty; mplus = (<|>); }

instance MonadError StructParseError ParseDictStruct where
  throwError = wrapParseDictStruct wrappedThrowError
  catchError = wrapParseDictStruct wrappedCatchError

instance MonadFail ParseDictStruct where { fail = wrappedMFail; }

instance StructParser ParseDictStruct where
  getParseState    = view dictContext <$> parseDictGetState
  putParseState    = parseDictModifyState . (dictContext .~)
  modifyParseState = parseDictModifyState . (dictContext %~)

parseDictGetState :: ParseDictStruct ParseDictStructState
parseDictGetState = ParseDictStruct $ lift get

parseDictModifyState :: (ParseDictStructState -> ParseDictStructState) -> ParseDictStruct ()
parseDictModifyState = ParseDictStruct . lift . modify

dictContext :: Lens' ParseDictStructState ParseStructState
dictContext = lens theDictContext $ \ a b -> a{ theDictContext = b }

dictSource :: Lens' ParseDictStructState Dictionary
dictSource = lens theDictSource $ \ a b -> a{ theDictSource = b }

-- | The 'parseDictionary' function is of type 'ParseListStruct' because a dictionary must first be
-- constructed from a list data structure. This function calls 'listToDictionary' then evaluates a
-- function of type 'ParseDictStruct'.
parseDictionary :: ParseDictStruct a -> ParseListStruct a
parseDictionary (ParseDictStruct f) =
  ParseDictStructState <$> (theListContext <$> parseListGetState) <*> listToDictionary >>=
  (throwError ||| return) . evalState (runExceptT f)

-- | Lookup a key in a 'Dictionary' and run a parser continuation on it.
--
-- Note that this function will remove the key (if it exists) from the dictionary, so evaluating
-- this function twice for the same key will fail on the second evaluation.
takeKeyAll :: Atom -> (NE.NonEmpty ListSlice -> ParseStruct a) -> ParseDictStruct a
takeKeyAll key f = do
  parseDictModifyState $ dictContext . structParserPath %~ ((StructIndexDict key) :)
  dictLookup key . theDictSource <$> parseDictGetState >>= \ case
    Nothing    -> mzero
    Just items -> do
      parseDictModifyState $ dictSource %~ dictDelete key
      getParseState >>= runParseStruct (f items)

-- | Lookup a key in a 'Dictionary' and run a parser on all items associated with this key.
--
-- Note that this function will remote the key (if it exists) from the dictionary, so evaluating
-- this function twice for the same key will fail on the second evaluation.
takeKey :: Atom -> (Int -> Struct -> ParseStruct a) -> ParseDictStruct (NE.NonEmpty a)
takeKey key f = takeKeyAll key $ mapM $ \ s -> f (theListSliceIndex s) (listSliceItem s)

-- | Lookup a key in a 'Dictionary' and run a parser on all items associated with this key. If there
-- is more than one item associated with this key, an exception is thrown.
--
-- Note that this function will remote the key (if it exists) from the dictionary, so evaluating
-- this function twice for the same key will fail on the second evaluation.
takeUnique :: Atom -> (Struct -> ParseStruct a) -> ParseDictStruct a
takeUnique key f = takeKeyAll key $ \ case
  a NE.:| [] -> getParseState >>= runParseStruct (f $ listSliceItem a)
  a NE.:| ax -> throwError $ StructParseError
    { structParseErrorType      = ""
    , structParseErrorInfo      = "key should be unique"
    , structParseErrorPath      = mempty
    , structParseErrorValue     = Nothing
    , structParseErrorNonUnique = Just $ NonUniqueKeyError
        { theOffendingKeyName      = key
        , theOffendingKeyInstances = a:ax
        }
    }

----------------------------------------------------------------------------------------------------

data FromLispPrim a
  = LispYield      a
  | LispWaitInput  (String -> [(FromLispPrim a, String)])
  | LispPrimSyntax !Strict.ByteString
    -- ^ Thrown when the string is invalid Lisp syntax.
  | LispPrimBad    !StructParseError
    -- ^ Thrown when the string is valid syntax but cannot be constructed by 'fromStruct'.
  deriving Functor

lispYieldedValue :: FromLispPrim a -> Bool
lispYieldedValue = \ case { LispYield{} -> True; _ -> False; }

forceFromLispExpr :: [(FromLispPrim a, String)] -> [(a, String)]
forceFromLispExpr = uncurry (++) . partition (lispYieldedValue . fst) >>> \ case
  (a, rem) : _ -> case a of
    LispYield      a   -> [(a, rem)]
    LispWaitInput{}    -> waiting rem
    LispPrimSyntax err -> error $ Strict.unpack err
    LispPrimBad    err -> error $ show err
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
  Map.insert "BEL"     '\a' $
  Map.insert "BS"      '\b' $
  Map.insert "HT"      '\t' $
  Map.insert "tab"     '\t' $
  Map.insert "LF"      '\n' $
  Map.insert "VT"      '\v' $
  Map.insert "FF"      '\f' $
  Map.insert "CR"      '\r' $
  Map.fromList (Vec.toList charTo3CharCode `zip` (chr <$> [0 ..]))

----------------------------------------------------------------------------------------------------

-- | The class of data types that can be converted to and from a Lisp-like "S-Expression".
class LispPrim a where
  toLispPrim   :: a -> String -> String
  fromLispPrim :: Int -> ReadS (FromLispPrim a)

-- Instances for 'LispPrim' are constructed below from the following combinators.

-- | Instantiate the 'fromLispPrim' function of the 'LispPrim' typeclass automatically using
-- the instance for 'Prelude.readsPrec' in the 'Prelude.Read' typeclass.
readsPrecLisp :: Read a => Int -> String -> [(FromLispPrim a, String)]
readsPrecLisp p = readsPrec p >=> \ (a, rem) -> [(LispYield a, rem)]

-- Since I'm not using a proper parser function, I'll need a map-like function for the 'ReadS' data
-- type.
readsMap :: (a -> b) -> ReadS a -> ReadS b
readsMap f m str = (\ (a, rem) -> (f a, rem)) <$> m str

rfmap :: Functor f => (a -> b) -> ReadS (f a) -> ReadS (f b)
rfmap f = readsMap (fmap f)

-- | The 'Prelude.String' type cannot instantiate the 'LispPrim' type because it overlaps
-- instances with the list type. You can use the 'LispPrim' instance for 'Strict.ByteString',
-- or you can use this function to convert a string directly to a lisp expression.
strToLispExpr :: String -> (String -> String)
strToLispExpr = shows

-- | The 'Prelude.String' type cannot instantiate the 'LispPrim' type because it overlaps
-- instances with the list type. You can use the 'LispPrim' instance for 'Strict.ByteString',
-- or you can use this function to convert a string directly to a lisp expression.
strFromLispExpr :: Int -> ReadS (FromLispPrim String)
strFromLispExpr p = readsPrecLisp p

seqToLisp :: Char -> Char -> (b -> [a]) -> (a -> String -> String) -> b -> String -> String
seqToLisp open close toList step = let cons = (:) in toList >>> \ case
  []   -> cons open . cons close
  a:ax -> let loop ax = case ax of { [] -> cons close; a:ax -> step a . loop ax; } in
    cons open . step a . loop ax
  
seqFromLisp
  :: Char -> Char
  -> ([a] -> b)
  -> (Int -> String -> [(FromLispPrim a, String)])
  -> Int -> String -> [(FromLispPrim b, String)]
seqFromLisp open close constr parse p = \ case { c:str | c == open -> loop id str; _ -> []; } where
  next stk (elem, str) = case elem of
    LispYield        a -> loop (stk . (a :)) str
    LispPrimSyntax err -> [(LispPrimSyntax err, str)]
    LispPrimBad    err -> [(LispPrimBad    err, str)]
    LispWaitInput cont -> [(LispWaitInput $ cont >=> next stk, str)]
  loop stk = sp >>> \ case
    c:str | c == close -> [(LispYield $ constr $ stk [], sp str)]
    str -> parse p str >>= next stk

sp :: String -> String
sp = dropWhile isSpace

subatomic :: Char -> Bool
subatomic c = isPrint c && not (isSpace c || elem c ("()[]{}#'`\";" :: [Char]))

-- Above were the 'LispPrim' combinators, now for the instances.

instance LispPrim Bool        where
  toLispPrim   true = ('#' :) . ((if true then 't' else 'f') :)
  fromLispPrim _ = \ case
    '#':tf:str | tf == 't' || tf == 'f' -> do
      str <- case str of { c:str | not (subatomic c) -> [sp str]; "" -> [""]; _ -> []; }
      [(LispYield $ if tf == 't' then True else False, str)]
    _ -> []

instance LispPrim Integer     where
  toLispPrim   = shows
  fromLispPrim = readsPrecLisp

instance LispPrim Char        where
  toLispPrim c = ("#\\" ++) . maybe (c :) ((++) . UTF8.toString) (charTo3CharCode Vec.!? ord c)
  fromLispPrim _p = \ case
    '#':'\\':str -> case span (not . isSpace) str of
      (""  , _  )  -> []
      (key', rem)  -> let key = UTF8.fromString key' in case Map.lookup key charFrom3CharCode of
        Nothing      -> case key' of
          [c]          -> [(LispYield c, rem)]
          _            ->
            [(LispPrimSyntax $ UTF8.fromString $ "unknown character label: "++show key', rem)]
        Just c       -> [(LispYield c, rem)]
    _            -> []

instance LispPrim Double      where
  toLispPrim   = shows
  fromLispPrim = readsPrecLisp

instance LispPrim Strict.ByteString where
  toLispPrim   = shows
  fromLispPrim = rfmap UTF8.fromString .
    (readsPrecLisp :: Int -> ReadS (FromLispPrim String))

instance LispPrim Atom        where
  toLispPrim     = shows
  fromLispPrim _ = pure . span (not . isSpace) >=> \ (a, rem) ->
    [(LispYield $ Atom $ UTF8.fromString a, rem)]

instance Structured a => LispPrim [a] where
  toLispPrim   = seqToLisp   '(' ')' id (toLispPrim . toStruct)
  fromLispPrim = seqFromLisp '(' ')' id $ \ p str -> do
    let next (a, rem) = case a of
          LispYield        a -> case fromStruct a of
                            Right  a -> [(LispYield     a, rem)]
                            Left err -> [(LispPrimBad err, rem)]
          LispPrimBad    err -> [(LispPrimBad    err, rem)]
          LispPrimSyntax err -> [(LispPrimSyntax err, rem)]
          LispWaitInput cont -> [(LispWaitInput $ cont >=> next, rem)]
    fromLispPrim p str >>= next

instance Structured a => LispPrim (Vec.Vector a) where
  toLispPrim   = toLispPrim . Vec.toList
  fromLispPrim = rfmap Vec.fromList . fromLispPrim

instance LispPrim Struct      where
  toLispPrim = \ case
    ObjFalse    -> toLispPrim False
    ObjTrue     -> toLispPrim True
    ObjInt    o -> toLispPrim o
    ObjChar   o -> toLispPrim o
    ObjFloat  o -> toLispPrim o
    ObjString o -> toLispPrim o
    ObjAtom   o -> toLispPrim o
    ObjList   o -> toLispPrim o
    ObjQuoted o -> ('\'' :) . toLispPrim o
    ObjHashed o -> ('#' :) . toLispPrim o
  fromLispPrim = rfmap (\ true -> if true then ObjTrue else ObjFalse) . fromLispPrim
      <> rfmap ObjInt    . fromLispPrim
      <> rfmap ObjChar   . fromLispPrim
      <> rfmap ObjFloat  . fromLispPrim
      <> rfmap ObjString . fromLispPrim
      <> rfmap ObjAtom   . fromLispPrim
      <> rfmap ObjList   . fromLispPrim
