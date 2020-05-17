-- | Automatic text processing, scripting tools.
module Hakshell.String
  ( tempTest,
    -- * Common Data Types
    StrictBytes, LazyBytes, CharVector,
    -- * Common String Functions
    ToStrictBytes(..),
    Packable(..), Unpackable(..), IntSized(..),
    SizePackable, packSize, -- <-NO (..), members are unsafe.
    -- * Pattern Matching
    StringPattern(..), findSubstring,
    Label, Offset, Capture(..), PatternMatch(..), Captured(..), matchVector,
    StringSlice, sliceOffset, sliceLength, sliceString, splitByPatternFrom,
    -- * Patterns
    ExactString(..), exact, 
    -- * Formatting integers
    BaseNumber, PaddingChar, NumCellWidth,
    digitSymbols, numberOfDigits, digitString,
    asBase2, asBase8, asBase10, asBase16L, asBase16U, asBase64,
    -- ** Custom tables for 'digitString'
    NumSymbolTable(..), 
    base2Syms, base8Syms, base10Syms, base16SymsL, base16SymsU, base64Syms,
  ) where

import           Hakshell.Struct

import           Control.Applicative
import           Control.Monad

import           Data.Maybe
import qualified Data.ByteString.Char8          as Strict
import qualified Data.ByteString.Lazy.Char8     as Lazy
import qualified Data.ByteString.Builder        as Build
import qualified Data.ByteString.Builder.Extra  as Bytes
import qualified Data.ByteString.UTF8           as StrictUTF8
import qualified Data.ByteString.Lazy.UTF8      as LazyUTF8
import qualified Data.IntMap                    as IMap
import           Data.Semigroup
import           Data.String
import           Data.Typeable
import qualified Data.Vector                    as Vec
import qualified Data.Vector.Unboxed            as UVec
import qualified Data.Vector.Unboxed.Mutable    as UMVec

import Debug.Trace

----------------------------------------------------------------------------------------------------

class ToStrictBytes str where { toStrictBytes :: str -> StrictBytes; }

-- | This class is a synonym for 'Data.String.fromString', renamed to the word 'pack' which is
-- easier to type.
class Packable str where { pack :: String -> str; }
instance Packable StrictBytes where { pack = StrictUTF8.fromString; }
instance Packable LazyBytes   where { pack = LazyUTF8.fromString; }
instance Packable CharVector  where { pack = UVec.fromList; }

-- | Inverse of 'Packable'.
class Unpackable str where { unpack :: str -> String; }
instance Unpackable StrictBytes where { unpack = StrictUTF8.toString; }
instance Unpackable LazyBytes   where { unpack = LazyUTF8.toString; }
instance Unpackable CharVector  where { unpack = UVec.toList; }

----------------------------------------------------------------------------------------------------

-- | Immutable objects that have an integer size or length that can be determined in O(1) time.
--
-- Note that 'StrictBytes' (strict 'Data.ByteString.ByteString') instantiates this 'IntSized' class
-- using the byte string length, and not the number of encoded UTF8 characters in the string, so
-- using 'intSize' to find the number of characters in an 'ExactString' may give results you are not
-- expecting. Use 'unpack' and then 'length' to get the number of characters, which is an O(n)
-- operation since the string needs to be decoded from UTF8 when this happens.
class IntSized obj where { intSize :: obj -> Int }
instance IntSized StrictBytes where { intSize = Strict.length; }
instance IntSized CharVector  where { intSize = UVec.length; }

----------------------------------------------------------------------------------------------------

-- | Immutable objects that can be constructed from a list with a known number of elements. This
-- operation is a little unsafe and prone to programmer-error because it requires you keep a proper
-- accounting of the length of the list. For lists that are too short to fill the @size::'Int'@
-- value given, the end of the string is padded with null (@'\NUL'@) characters.
class SizePackable obj where { packSize :: Int -> String -> obj; }

instance SizePackable LazyBytes where
  packSize size =
    Bytes.toLazyByteStringWith (Bytes.safeStrategy size size) mempty .
    Build.stringUtf8

instance SizePackable StrictBytes where
  packSize size = Lazy.toStrict . packSize size

instance SizePackable CharVector where
  packSize size chars = UVec.create
    (do vec <- UMVec.new size
        let len      = length chars
        let padding  = replicate (max 0 $ size - len) '\0'
        let fill vec = fmap (const vec) $
              mapM_ (uncurry $ UMVec.write vec) $ zip [0 ..] (chars ++ padding)
        fill vec
        if len <= size then return vec else UMVec.new len >>= fill
    )

----------------------------------------------------------------------------------------------------

-- | A strict 'Strict.ByteString' from the "Data.ByteString.Char8" module. UTF8 encoding is provided
-- by the "Data.ByteString.UTF8" module.
type StrictBytes = Strict.ByteString

-- | A lazy 'Lazy.ByteString' from the "Data.ByteString.Lazy.Char8" module. UTF8 encoding is
-- provided by the "Data.ByteString.Lazy.UTF8" module.
type LazyBytes   = Lazy.ByteString

-- | An immutable unboxed vector that contains a sequence of 'Char' values in contiguous
-- memory. This data structure in many (but not all) cases tends to be the most efficient way to
-- store strings. Unlike 'StrictBytes' or 'LazyBytes', a 'CharVector' guarnatees O(1) random access
-- to characters in the array. So for strings that are subject to very heavy analycial computations
-- in which the results cannot be computed by simple folding and mapping functions, a 'CharVector'
-- may provide better performance at the expense of possibly increased memory usage and possibly
-- slower memory access.
type CharVector  = UVec.Vector Char

----------------------------------------------------------------------------------------------------

class StringPattern pat where
  -- | Match the beginning of a target string.
  matchHead     :: Capture -> pat -> StrictBytes -> PatternMatch
  -- | Match once somewhere in the middle of a target string, searching from beginning.
  findSubstringFrom :: Capture -> pat -> Offset -> StrictBytes -> PatternMatch
  -- | Returns the smallest possible string that can be matched by the @pat@ pattern.
  smallestMatch :: pat -> Int

findSubstring :: StringPattern pat => Capture -> pat -> StrictBytes -> PatternMatch
findSubstring cap pat = findSubstringFrom cap pat 0

-- | Find all non-overlapping matching 'StringPattern's in a given 'StrictBytes' string. Returns an
-- assoication list indexed by the 'sliceOffset' of the 'StringSlice' of each 'Captured' item.
splitByPatternFrom
  :: StringPattern pat
  => pat -> Offset -> StrictBytes -> IMap.IntMap (Vec.Vector Captured)
splitByPatternFrom pat offset str = loop IMap.empty offset where
  loop map offset = case findSubstringFrom Capture pat offset str of
    PatternNoMatch skip -> loop map $ offset + max 1 skip
    PatternMatch    vec -> loop (IMap.insert offset vec map) $ offset +
      maximum (1 : (intSize . capturedSlice <$> Vec.toList vec))

----------------------------------------------------------------------------------------------------

type Label  = StrictBytes
type Offset = Int

-- | This is a way of commanding the 'findSubstringFrom' function
data Capture
  = NoMatch
    -- ^ if 'StringPattern' matches, evaluate to 'PatternNoMatch'
  | CaptureNoMatch
    -- ^ Return a capture group containing the whole string if the given pattern does not match.
  | CaptureNamedNoMatch !Label
    -- ^ Return a named capture group containing the whole string if the given pattern does not
    -- match.
  | Match
    -- ^ if 'StringPattern' matches, evaluate to 'PatternMatch'
  | Capture
    -- ^ if 'StringPattern' matches, evaluate to 'PatternMatch' with 'Captured'. Be careful not to
    -- confuse this constructor with the 'Captured' constructor (ends with a @d@). 'Capture' is a
    -- present tense, 'Captured' is past tense, as in the 'Capture' function has already completed.
  | CaptureNamed !Label
    -- ^ like 'Capture' but evaluate to 'PatternMatch' with 'CapturedName'. Like 'Capture' and
    -- 'Captured', be careful not to confuse this constructor with 'CapturedNamed'. Again, this
    -- function is present tense, whereas 'CapturedNamed' is past tense.
  deriving (Eq, Ord, Typeable)

-- | A value determining whether a 'StringPattern' has matched a string input. This value
-- instantiates 'Data.Semigroup.Semigroup' and 'Data.Monoid.Monoid', so you can append multiple
-- matches from multiple patterns sequential matched against a string input.
data PatternMatch
  = PatternNoMatch !Int
    -- ^ The integer value is used when trying to match a pattern anywhere in a target string, it
    -- should indicate how many characters indicies @i@ you can skip over because it is
    -- mathematically impossible to match the pattern anywhere between the current index and
    -- anywhere within @n@ indicies before.
  | PatternMatch (Vec.Vector Captured)
  deriving (Eq, Ord, Show)

data Captured
  = Captured{ capturedSlice :: !StringSlice }
  | CapturedNamed{ capturedLabel :: !Label, capturedSlice :: !StringSlice }
  deriving (Eq, Ord, Show)

-- | An index into a 'StrictBytes' string. This data type is usually the result of a pattern match.
data StringSlice
  = StringSlice
    { sliceOffset :: !Offset
      -- ^ The starting index of the slice.
    , sliceLength :: !Int
      -- ^ The number of bytes after the 'sliceStart' index.
    , sliceString :: !StrictBytes
      -- ^ The string into which the 'sliceStart' and 'sliceEnd' are pointing.
    }
  deriving (Eq, Ord, Show, Typeable)

instance IntSized StringSlice where { intSize = sliceLength; }

instance Structured PatternMatch where
  toStruct = \ case
    PatternNoMatch i -> ObjList $ Vec.fromListN 3
      [ObjAtom _pat_match, ObjFalse, ObjInt $ toInteger i]
    PatternMatch vec -> ObjList $ Vec.fromListN (2 + Vec.length vec) $
      [ObjAtom _pat_match, ObjTrue] ++ (toStruct <$> Vec.toList vec)
  parseStruct = parseList $ annotateParse "pattern-match-result" $ do
    takeAtom $ guard . (== _pat_match)
    bool <- takeStruct $ structRequire "boolean indicating whether pattern matched" parseStruct
    if bool then takeSliceToEnd $ do
        n <- getStructLength
        PatternMatch . Vec.fromListN n <$> many (takeElem pure)
      else do
        skip <- takeStruct $
          structRequire "integer indicating number of characters to skip" parseStruct
        (takeEndOfList >> return (PatternNoMatch skip)) <|>
          fail "too many items for (matched? false ...) constructor"

_pat_match :: Atom
_pat_match = read "matched?"

instance Structured Captured where
  toStruct = ObjList . Vec.fromList . \ case
    Captured          slice -> capturedSliceToList slice
    CapturedNamed lbl slice -> ObjString lbl : capturedSliceToList slice
  parseStruct = parseList $ do
    lbl <- optional $ takeString pure
    off <- takeInt $ pure . fromInteger
    str <- takeString pure
    let slice = StringSlice
          { sliceOffset = off
          , sliceLength = Strict.length str
          , sliceString = str
          }
    return $ maybe (Captured slice) (flip CapturedNamed slice) lbl

capturedSliceToList :: StringSlice -> [Struct]
capturedSliceToList s = let off = sliceOffset s in
  [ ObjInt $ toInteger off
  , ObjString $ Strict.take (sliceLength s) $ Strict.drop off $ sliceString s
  ]

instance Structured StringSlice where
  toStruct s = ObjList $ Vec.fromList
    [ ObjAtom _str_slice
    , objInt $ sliceOffset s
    , objInt $ sliceLength s
    , objString $ sliceString s
    ]
  parseStruct = parseList $ do
    takeAtom $ guard . (== _str_slice)
    off <- takeElem pure
    len <- takeElem pure
    str <- takeElem pure
    return StringSlice
      { sliceOffset = off
      , sliceLength = len
      , sliceString = str
      }

_str_slice :: Atom
_str_slice = read "slice"

instance ToStrictBytes StringSlice where
  toStrictBytes s = Strict.take (sliceLength s) $ Strict.drop (sliceOffset s) $ sliceString s

instance ToStrictBytes Captured where { toStrictBytes = toStrictBytes . capturedSlice; }

instance Semigroup PatternMatch where
  a <> b = case (a, b) of
    (PatternNoMatch a, PatternNoMatch b) -> PatternNoMatch $ max a b
    _                                    -> PatternMatch $ matchVector a <> matchVector b
  sconcat = PatternMatch . sconcat . fmap matchVector

instance Monoid PatternMatch where
  mempty  = PatternMatch mempty
  mappend = (<>)
  mconcat = PatternMatch . mconcat . fmap matchVector

-- | Convert a 'PatternMatch' to a vector of 'Captured' strings.
matchVector :: PatternMatch -> Vec.Vector Captured
matchVector = \ case
  PatternMatch vec -> vec
  PatternNoMatch{} -> Vec.empty

----------------------------------------------------------------------------------------------------

-- | A string that instantiates the 'StringPattern' class. Use the 'str' constructor to create an
-- 'ExactString' value.
--
-- Note that this data type instantiates 'IntSized' using the byte string length, and not the number
-- of encoded UTF8 characters in the string, meaning characters that require more than one byte to
-- be encoded in UTF8 will count as two elements. The bottom line is that using 'intSize' to find
-- the number of characters in an 'ExactString' may give results you are not expecting. Use 'unpack'
-- and then 'length' to get the number of characters, which is an O(n) operation since the string
-- needs to be decoded from UTF8 when this happens.
newtype ExactString = ExactString Strict.ByteString
  deriving (Eq, Ord, Typeable)

-- | Shorter name for the 'ExactString' constructor.
exact :: Strict.ByteString -> ExactString
exact = ExactString

instance Packable     ExactString where { pack = ExactString . Strict.pack; }
instance IsString     ExactString where { fromString = pack; }
instance Unpackable   ExactString where { unpack  (ExactString str) = Strict.unpack str; }
instance IntSized     ExactString where { intSize (ExactString str) = Strict.length str; }
instance SizePackable ExactString where
  packSize size = ExactString . packSize size

instance Semigroup    ExactString where
  (ExactString a) <> (ExactString b) = ExactString (a <> b)
  sconcat = ExactString . sconcat . fmap (\ (ExactString a) -> a)

instance Monoid       ExactString where
  mempty  = ExactString mempty
  mappend = (<>)
  mconcat = ExactString . mconcat . fmap (\ (ExactString a) -> a)

instance Structured ExactString where
  toStruct (ExactString str) = ObjList $ Vec.fromList
    [ObjAtom $ fromJust $ toAtom "exact", ObjString str]
  parseStruct = parseList $ do
    takeAtom (\ a -> guard (fromJust (toAtom "exact") == a) >> pure a)
    takeString (pure . ExactString)

instance StringPattern ExactString where
  smallestMatch = intSize
  matchHead capt (ExactString pat) str =
    if Strict.isPrefixOf pat str then
      let match constr =
            PatternMatch $ Vec.singleton $ constr $ StringSlice 0 (Strict.length pat) str
      in  case capt of
            Match              -> PatternMatch mempty
            Capture            -> match Captured
            CaptureNamed label -> match $ CapturedNamed label
            _                  -> PatternNoMatch (error "TODO: how many things to skip")
    else  case capt of
            NoMatch            -> PatternMatch mempty
            _                  -> PatternNoMatch (error "TODO: how many things to skip")
  findSubstringFrom = findExactSubstringFrom

----------------------------------------------------------------------------------------------------

-- The base of a number, e.g. 10 to get digits 0-9, 2 to get binary digits.
type BaseNumber = Int

-- | When aligning numbers to the right of a column in a matrix of characters (like a text console),
-- it is neccessary to specify the width of the column so that an appropriate number of padding
-- characters can be inserted before the number. If a number is larger than this value, it is
-- printed as it is (without truncating) which can throw off the column alignment.
type NumCellWidth = Int

-- | A character used to fill space, usually just a space character.
type PaddingChar = Char

data NumSymbolTable
  = NumSymbolTable
    { numBase         :: !BaseNumber
    , numPaddingChar  :: !PaddingChar
    , numSymbolLookup :: Int -> Char
    }

-- | The function @digitSymbols n base@ divides the number @n@ by the @base@ repeatedly, creating a
-- list of digits which you can then convert to characters. To convert the number to a strinng,
-- apply the output of this function to 'digitString'.
digitSymbols
  :: (Integral n, Ord n)
  => n -- ^ the number to deconstruct
  -> BaseNumber -- ^ the base of the number representation
  -> [Int] -- ^ the list of symbols used to represent the number of type @n@
digitSymbols n ibase = loop [] n where
  base = fromIntegral ibase
  loop stk n0 =
    if n0 < base then fromIntegral n0 : stk else
    let (n, r) = divMod n0 base in loop (fromIntegral r : stk) n
{-# INLINE digitSymbols #-}

-- | The function @numberOfDigits n base@ computes the minimum number of digits necessary to
-- represent a number in that base. This is useful for computing alignments of numbers, because you
-- can obtain the string length a number will be before it is printed.
--
-- This function simply takes the length of the list returned by 'digitSymbols'.
numberOfDigits :: (Integral n, Ord n) => n -> BaseNumber -> Int
numberOfDigits = (.) length . digitSymbols
{-# INLINE numberOfDigits #-}

-- | Calls 'digitSymbols', then maps these symbols to the 'Char's in the given vector. Pads the
-- string with space characters so the number aligns to the right of a cell in a column of
-- numbers. The base of the number is taken from the length of the 'CharVector' symbol table.
digitString
  :: (Integral n, Ord n, Packable str)
  => NumSymbolTable
  -- ^ Map 'digitSymbols' to the characters in this array, can be created with 'packSize'
  -> NumCellWidth -- ^ the cell width when algning a number to the right.
  -> n            -- ^ the number to deconstruct
  -> str
digitString table width n = pack (padding ++ symbols) where
  symbols = numSymbolLookup table <$> digitSymbols n (numBase table)
  padding = replicate (max 0 $ width - length symbols) (numPaddingChar table)
{-# NOINLINE digitString #-}

asBase2 :: (Integral n, Ord n, Packable str) => NumCellWidth -> n -> str
asBase2 = digitString base2Syms
{-# NOINLINE asBase2 #-}

asBase8 :: (Integral n, Ord n, Packable str) => NumCellWidth -> n -> str
asBase8 = digitString base8Syms
{-# NOINLINE asBase8 #-}

asBase10 :: (Integral n, Ord n, Packable str) => NumCellWidth -> n -> str
asBase10 = digitString base10Syms
{-# NOINLINE asBase10 #-}

asBase16L :: (Integral n, Ord n, Packable str) => NumCellWidth -> n -> str
asBase16L = digitString base16SymsL
{-# NOINLINE asBase16L #-}

asBase16U :: (Integral n, Ord n, Packable str) => NumCellWidth -> n -> str
asBase16U = digitString base16SymsU
{-# NOINLINE asBase16U #-}

asBase64 :: (Integral n, Ord n, Packable str) => NumCellWidth -> n -> str
asBase64 = digitString base64Syms
{-# NOINLINE asBase64 #-}

publicNumSymTable :: NumSymbolTable -> NumSymbolTable
publicNumSymTable table =
  table{ numSymbolLookup = numSymbolLookup table . (`mod` (numBase table)) }

base2Syms :: NumSymbolTable
base2Syms = NumSymbolTable
  { numBase = 2
  , numPaddingChar  = ' '
  , numSymbolLookup = \ i -> if i == 0 then '0' else '1'
  }
{-# INLINE base2Syms #-}

base8Syms :: NumSymbolTable
base8Syms = publicNumSymTable _base8Syms
{-# INLINE base8Syms #-}

_base8Syms :: NumSymbolTable
_base8Syms = base2Syms{ numBase = 8, numSymbolLookup = (table UVec.!) } where
  table = mkSymCharTable 10 ['0' .. '7']
{-# NOINLINE _base8Syms #-}

base10Syms :: NumSymbolTable
base10Syms = publicNumSymTable _base10Syms
{-# INLINE base10Syms #-}

_base10Syms :: NumSymbolTable
_base10Syms = base2Syms{ numBase = 10, numSymbolLookup = (table UVec.!)} where
  table = mkSymCharTable 10 ['0' .. '9']
{-# NOINLINE _base10Syms #-}

-- | Upper-case base-16 symbols
base16SymsU :: NumSymbolTable
base16SymsU = publicNumSymTable _base16SymsU
{-# INLINE base16SymsU #-}

_base16SymsU :: NumSymbolTable
_base16SymsU = base2Syms{ numBase = 16, numSymbolLookup = (table UVec.!)} where
  table = mkSymCharTable 16 $ ['0' .. '9'] ++ ['A' .. 'F']
{-# NOINLINE _base16SymsU #-}

-- | Lower-case base-16 symbols
base16SymsL :: NumSymbolTable
base16SymsL = publicNumSymTable _base16SymsL
{-# INLINE base16SymsL #-}

_base16SymsL :: NumSymbolTable
_base16SymsL = base2Syms{ numBase = 16, numSymbolLookup = (table UVec.!) } where
  table = mkSymCharTable 16 $ ['0' .. '9'] ++ ['a' .. 'f']
{-# NOINLINE _base16SymsL #-}

base64Syms :: NumSymbolTable
base64Syms = publicNumSymTable _base64Syms
{-# INLINE base64Syms #-}

_base64Syms :: NumSymbolTable
_base64Syms = base2Syms{ numBase = 64, numSymbolLookup = (table UVec.!) } where
  table = mkSymCharTable 64 $ concat [['A'..'Z'],['a'..'z'],['0'..'9'],"+/"]
{-# NOINLINE _base64Syms #-}

_numSymCharTable :: CharVector -> NumSymbolTable
_numSymCharTable table = base2Syms{ numBase = intSize table, numSymbolLookup = (table UVec.!) }

-- not for export 
--
-- Create a CharVector containing the symbols that can passed to 'digitString' to map the
-- 'digitSymbols's of a number.
mkSymCharTable :: BaseNumber -> String -> CharVector
mkSymCharTable base chars = UVec.create
  (do vec <- UMVec.new base
      mapM_ (uncurry $ UMVec.write vec) $ zip [0 ..] chars
      return vec
  )

----------------------------------------------------------------------------------------------------

enableTrace :: Bool
enableTrace = False

eq, gt, le :: (Eq a, Ord a) => (String, a -> a -> Bool)
eq = ("==", (==))
gt = (">", (>))
le = ("<=", (<=))

tr :: String -> more -> more
tr msg = if enableTrace then trace msg else id

val :: Show a => String -> a -> String
val lbl val = " (" ++ lbl ++ '=' : show val ++ ")"

cmp
  :: (Eq a, Eq b, Show a, Show b)
  => (String, a -> b -> Bool) -> String -> a -> String -> b -> String
cmp (cmpstr, cmp) lblA a lblB b = " (" ++
  lblA ++ '=' : show a ++ ' ' : cmpstr ++ ' ' : lblB ++ '=' : show b ++
  " -> " ++ (if cmp a b then "yes" else "no") ++ ")"

-- An implementation of the Boyer-Moore algorithm that does not produce a shift lookup table for the
-- pattern. Rather than using a table, a search for the offset of the bad character is performed in
-- the pattern between the start of the good suffix and the start of the pattern string every time a
-- bad character is encountered. Given that the patterns that are typically used in shell scripts
-- are usually rather small (often less than 16 bytes in length), and that the haystacks that are
-- searched are also small (typically in the vicinity of 128 bytes, less than the size of the lookup
-- table) this implementation is fast enough for the purposes of this library without using a shift
-- lookup table, and may even be faster than a similar implementation that did use a shift lookup
-- table.
findExactSubstringFrom :: Capture -> ExactString -> Offset -> StrictBytes -> PatternMatch
findExactSubstringFrom capt (ExactString pat) offset str =
  if offset < 0 then noMatch (abs offset) else loop offset where
    --index  = Strict.unsafeIndex else Strict.index
    index  = Strict.index
    strlen = Strict.length str
    patlen = Strict.length pat
    strmatch constr offset = PatternMatch $ Vec.singleton $ constr
      StringSlice{ sliceOffset = offset, sliceLength = patlen, sliceString = str }
    shift mark cS p0 =
      tr ("shift:" ++ val "mark" mark ++ val "cS" cS ++ val "p0" p0 ++ cmp le "p0" p0 "z" 0) $
      if p0 <= 0 then patlen else
      let { p = p0 - 1; cP = index pat p; } in
      tr ("shift:" ++ cmp eq ("pat["++show p++"]") cP "cP" cP) $
      if cP == cS then tr (val "<- (mark - p)" (mark - p)) $ mark - p else
      if p <= 0 then tr (val "p" p ++ "<= 0 -> yes <- mark="++show mark) mark else
      shift mark cS p
    suffix offset p0 s0 =
      tr ("suffix:" ++ val "offset" offset ++ val "p0" p0 ++ val "s0" s0 ++ cmp le "p0" p0 "z" 0) $
      if p0 <= 0 then Right () else
      let { s = s0 - 1; p = p0 - 1; cS = index str s; cP = index pat p } in
      tr ("suffix:" ++ cmp eq ("str["++show s++"]") cS ("pat["++show p++"]") cP) $
      if cP == cS then suffix offset p s else Left $ shift p cS p
    noMatch minoffset = case capt of
      NoMatch                  -> PatternMatch mempty
      CaptureNoMatch           -> strmatch Captured offset
      CaptureNamedNoMatch name -> strmatch (CapturedNamed name) offset
      _                        -> PatternNoMatch minoffset
    loop      offset =
      let strtop = offset + patlen in
      tr ("loop" ++ val "offset" offset ++ cmp gt "strtop" strtop "strlen" strlen) $
      if strtop > strlen then noMatch strlen else case suffix offset patlen strtop of
        Left  i  -> tr ("loop <- Left "++show i) $ loop $ offset + i
        Right () -> case capt of
          Match             -> PatternMatch mempty
          Capture           -> strmatch Captured offset
          CaptureNamed name -> strmatch (CapturedNamed name) offset
          _                 -> PatternNoMatch 1

-- | Temporary test.
tempTest :: IO ()
tempTest = mapM_ check tests where
  check :: (StrictBytes, StrictBytes, StrictBytes -> StrictBytes -> PatternMatch) -> IO ()
  check (haystack, needle, expecting) = do
    let result = findExactSubstringFrom Capture (ExactString needle) 0 haystack
    let expected = expecting haystack needle
    let pass = result == expected
    (if pass then putStrLn else error) $
      "in "++show haystack++" find "++show needle++" -> "++
      if pass then "OK" else
      "FAIL\nexpected = "++show expected++"\n  result = "++show result
  noMatch i _str _pat = PatternNoMatch i
  match i str pat = PatternMatch $ Vec.singleton $ Captured (StringSlice i (Strict.length pat) str)
  tests =
    [ ("", "", match 0)
    , ("a", "a", match 0)
    , ("a", "b", noMatch 1)
    , ("a", "", match 0)
    , ("", "a", noMatch 0)
    , ("ab", "a", match 0)
    , ("ab", "b", match 1)
    , ("ab", "c", noMatch 1)
    , ("ab", "", match 0)
    , ("abc", "a", match 0)
    , ("abc", "b", match 1)
    , ("abc", "c", match 2)
    , ("abc", "ab", match 0)
    , ("abc", "bc", match 1)
    , ("abc", "abc", match 0)
    , ("abc", "xbc", noMatch 3)
    , ("abc", "abx", noMatch 1)
    , ("abc", "ax", noMatch 1)
    , ("abc", "xb", noMatch 2)
    , ("abc", "xc", noMatch 2)
    , ("abc", "x", noMatch 1)
    , ("aaaabaaa", "abaaa", match 3)
    , ("abcdefghijklmno", "hijkl", match 7)
    , ("abcdeabcdeabcdef", "abcdef", match 10)
    , ("bxxxxxxaaxxxxxxa", "axxxxxxa", match 8)
    ]
