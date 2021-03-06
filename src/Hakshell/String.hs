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
  ) where

import           Hakshell.Struct

import           Control.Monad

import           Data.String
import qualified Data.ByteString.Char8          as Strict
import qualified Data.ByteString.Lazy.Char8     as Lazy
import qualified Data.ByteString.Builder        as Build
import qualified Data.ByteString.Builder.Extra  as Bytes
import qualified Data.ByteString.UTF8           as StrictUTF8
import qualified Data.ByteString.Lazy.UTF8      as LazyUTF8
import qualified Data.IntMap                    as IMap
import           Data.Semigroup
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
  packSize size elems = UVec.create
    (do vec <- UMVec.replicate size '\0'
        forM_ (zip [0 ..] elems) $ uncurry $ UMVec.write vec
        return vec
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
  => StrictBytes -> pat -> Offset -> StrictBytes -> IMap.IntMap Captured
splitByPatternFrom str pat = loop id where
  loop stk offset str = case findSubstringFrom Capture pat offset str of
    _ -> error "TODO: splitBy"

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
  = PatternNoMatch
  | PatternMatch (Vec.Vector Captured)
  deriving (Eq, Ord, Show)

data Captured
  = Captured{ capturedSlice :: !StringSlice}
  | CapturedNamed{ capturedLabel :: !Label , capturedSlice :: !StringSlice}
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

instance ToStrictBytes StringSlice where
  toStrictBytes s = Strict.take (sliceLength s) $ Strict.drop (sliceOffset s) $ sliceString s

instance ToStrictBytes Captured where { toStrictBytes = toStrictBytes . capturedSlice; }

instance Semigroup PatternMatch where
  a <> b = case (a, b) of
    (PatternNoMatch, PatternNoMatch) -> PatternNoMatch
    _                                -> PatternMatch $ matchVector a <> matchVector b
  sconcat = PatternMatch . sconcat . fmap matchVector

instance Monoid PatternMatch where
  mempty  = PatternMatch mempty
  mappend = (<>)
  mconcat = PatternMatch . mconcat . fmap matchVector

-- | Convert a 'PatternMatch' to a vector of 'Captured' strings.
matchVector :: PatternMatch -> Vec.Vector Captured
matchVector = \ case
  PatternMatch vec -> vec
  PatternNoMatch   -> Vec.empty

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
            _                  -> PatternNoMatch
    else  case capt of
            NoMatch            -> PatternMatch mempty
            _                  -> PatternNoMatch
  findSubstringFrom = findExactSubstringFrom

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
  if offset < 0 then noMatch else loop offset where
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
    noMatch          = case capt of
      NoMatch                  -> PatternMatch mempty
      CaptureNoMatch           -> strmatch Captured offset
      CaptureNamedNoMatch name -> strmatch (CapturedNamed name) offset
      _                        -> PatternNoMatch
    loop      offset =
      let strtop = offset + patlen in
      tr ("loop" ++ val "offset" offset ++ cmp gt "strtop" strtop "strlen" strlen) $
      if strtop > strlen then noMatch else case suffix offset patlen strtop of
        Left  i  -> tr ("loop <- Left "++show i) $ loop $ offset + i
        Right () -> case capt of
          Match             -> PatternMatch mempty
          Capture           -> strmatch Captured offset
          CaptureNamed name -> strmatch (CapturedNamed name) offset
          _                 -> PatternNoMatch

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
  noMatch _str _pat = PatternNoMatch
  match i str pat = PatternMatch $ Vec.singleton $ Captured (StringSlice i (Strict.length pat) str)
  tests =
    [ ("", "", match 0)
    , ("a", "a", match 0)
    , ("a", "b", noMatch)
    , ("a", "", match 0)
    , ("", "a", noMatch)
    , ("ab", "a", match 0)
    , ("ab", "b", match 1)
    , ("ab", "c", noMatch)
    , ("ab", "", match 0)
    , ("abc", "a", match 0)
    , ("abc", "b", match 1)
    , ("abc", "c", match 2)
    , ("abc", "ab", match 0)
    , ("abc", "bc", match 1)
    , ("abc", "abc", match 0)
    , ("abc", "xbc", noMatch)
    , ("abc", "abx", noMatch)
    , ("abc", "ax", noMatch)
    , ("abc", "xb", noMatch)
    , ("abc", "xc", noMatch)
    , ("abc", "x", noMatch)
    , ("aaaabaaa", "abaaa", match 3)
    , ("abcdefghijklmno", "hijkl", match 7)
    , ("abcdeabcdeabcdef", "abcdef", match 10)
    , ("bxxxxxxaaxxxxxxa", "axxxxxxa", match 8)
    ]
