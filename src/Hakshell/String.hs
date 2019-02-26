-- | Automatic text processing, scripting tools.
module Hakshell.String
  ( tempTest,
    StrictBytes, LazyBytes, ToByteString(..),
    Packable(..), Unpackable(..), IntSized(..),
    SizePackable, -- <-NO (..), members are unsafe.
    -- * Pattern Matching
    StringPattern(..),
    Label, Offset, Capture(..), PatternMatch(..), Captured(..), matchVector,
    StringSlice, sliceStart, sliceSize, sliceString, splitBy,
    -- * Patterns
    ExactString(..), exact, 
  ) where

import           Hakshell.Struct

import           Data.String
import qualified Data.ByteString.Char8          as Strict
import qualified Data.ByteString.Lazy.Char8     as Lazy
import qualified Data.ByteString.Builder        as Build
import qualified Data.ByteString.Builder.Extra  as Bytes
--import qualified Data.ByteString.UTF8           as StrictUTF8
import qualified Data.ByteString.Lazy.UTF8      as LazyUTF8
import qualified Data.IntMap                    as IMap
--import           Data.Monoid
import           Data.Semigroup
import           Data.Typeable
import qualified Data.Vector                    as Vec

----------------------------------------------------------------------------------------------------

type StrictBytes = Strict.ByteString
type LazyBytes   = Lazy.ByteString

class ToByteString str where { toByteString :: str -> StrictBytes; }

-- | This class is a synonym for 'Data.String.fromString', renamed to the word 'pack' which is
-- easier to type.
class Packable str where { pack :: String -> str; }
instance Packable StrictBytes where { pack = Strict.pack; }
instance Packable LazyBytes   where { pack = LazyUTF8.fromString; }

-- | Inverse of 'Packable'.
class Unpackable str where { unpack :: str -> String; }
instance Unpackable StrictBytes where { unpack = Strict.unpack; }
instance Unpackable LazyBytes   where { unpack = Lazy.unpack; }

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

----------------------------------------------------------------------------------------------------

class StringPattern pat where
  -- | Match the beginning of a target string.
  matchHead     :: Capture -> pat -> StrictBytes -> PatternMatch
  -- | Match once somewhere in the middle of a target string, searching from beginning.
  findSubstringFrom :: Capture -> pat -> Offset -> StrictBytes -> PatternMatch
  -- | Returns the smallest possible string that can be matched by the @pat@ pattern.
  smallestMatch :: pat -> Int

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
    -- ^ if 'StringPattern' matches, evaluate to 'PatternMatch' with 'Captured'
  | CaptureNamed !Label
    -- ^ like 'Capture' but evaluate to 'PatternMatch' with 'CapturedName'
  deriving (Eq, Ord, Typeable)

-- | A value determining whether a 'StringPattern' has matched a string input. This value
-- instantiates 'Data.Semigroup.Semigroup' and 'Data.Monoid.Monoid', so you can append multiple
-- matches from multiple patterns sequential matched against a string input.
data PatternMatch
  = PatternNoMatch
  | PatternMatch (Vec.Vector Captured)
  deriving (Eq, Ord)

data Captured
  = Captured
    { capturedOffset :: !Offset
    , capturedString :: !StrictBytes
    }
  | NamedCaptured
    { capturedLabel  :: !Label
    , capturedOffset :: !Offset
    , capturedString :: !StrictBytes
    }
  deriving (Eq, Ord)

instance ToByteString Captured where { toByteString = capturedString; }

instance Semigroup PatternMatch where
  a <> b = case (a, b) of
    (PatternNoMatch, PatternNoMatch) -> PatternNoMatch
    _                                -> PatternMatch $ matchVector a <> matchVector b
  sconcat = PatternMatch . sconcat . fmap matchVector

instance Monoid PatternMatch where
  mempty  = PatternMatch mempty
  mappend = (<>)
  mconcat = PatternMatch . mconcat . fmap matchVector

matchVector :: PatternMatch -> Vec.Vector Captured
matchVector = \ case
  PatternMatch vec -> vec
  PatternNoMatch   -> Vec.empty

----------------------------------------------------------------------------------------------------

-- | An index into a 'StrictBytes' string. This data type is usually the result of a pattern match.
data StringSlice
  = StringSlice
    { sliceStart  :: !Offset
      -- ^ The starting index of the slice.
    , sliceSize   :: !Int
      -- ^ The number of bytes after the 'sliceStart' index.
    , sliceString :: !StrictBytes
      -- ^ The string into which the 'sliceStart' and 'sliceEnd' are pointing.
    }
  deriving (Eq, Ord, Typeable)

splitBy :: StringPattern pat => StrictBytes -> pat -> IMap.IntMap StringSlice
splitBy = error "TODO"

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
    if Strict.isPrefixOf pat str then case capt of
        NoMatch            -> PatternNoMatch
        Match              -> PatternMatch mempty
        Capture            -> PatternMatch $ Vec.singleton $ Captured 0 pat
        CaptureNamed label -> PatternMatch $ Vec.singleton $ NamedCaptured label 0 pat
      else case capt of
        NoMatch            -> PatternMatch mempty
        _                  -> PatternNoMatch
  findSubstringFrom = findExactSubstringFrom

findExactSubstringFrom :: Capture -> ExactString -> Offset -> StrictBytes -> PatternMatch
findExactSubstringFrom capt (ExactString pat) offset str =
  if offset < 0 then noMatch else loop offset where
    strlen           = Strict.length str
    patlen           = Strict.length pat
    strmatch  constr = PatternMatch $ Vec.singleton $ constr offset str
    patscan mark c i = if i <= 0 then mark else
      if Strict.index pat i == c then mark - i else patscan mark c $! i - 1
    scan  i0 offset0 = seq offset $! if i0 == 0 then Right () else
      let { i = i0 - 1; offset = offset0 - 1; c = Strict.index str offset; } in
      if Strict.index pat i == c then scan i offset else Left $ patscan i c $! i - 1
    noMatch          = case capt of
      NoMatch                  -> PatternMatch mempty
      CaptureNoMatch           -> strmatch Captured
      CaptureNamedNoMatch name -> strmatch (NamedCaptured name)
      _                        -> PatternNoMatch
    loop      offset = if offset > strlen then noMatch else case scan patlen offset of
      Left  i  -> loop $! offset + i
      Right () -> case capt of
        Match             -> PatternMatch mempty
        Capture           -> strmatch Captured
        CaptureNamed name -> strmatch (NamedCaptured name)
        _                 -> PatternNoMatch

-- | Temporary test.
tempTest :: IO ()
tempTest = mapM_ match tests where
  showCapture = \ case
    Captured{ capturedOffset=off } -> show off
    NamedCaptured{ capturedLabel=lbl, capturedOffset=off } -> unpack lbl++"="++show off
  match (haystack, needle, expected) = do
    let end result msg = (if result expected then putStrLn else error) $ msg
    putStr $ "in "++show haystack++" find "++show needle++" -> "
    case findExactSubstringFrom Capture (ExactString needle) 0 haystack of
      PatternNoMatch   -> end not "NOT FOUND"
      PatternMatch vec -> end id $ "FOUND " ++ unwords (showCapture <$> Vec.toList vec)
  tests =
    [ ("", "", True)
    , ("a", "a", True)
    , ("a", "b", False)
    , ("a", "", False)
    , ("ab", "a", True)
    , ("ab", "b", True)
    , ("ab", "c", False)
    , ("ab", "", True)
    , ("abc", "a", True)
    , ("abc", "b", True)
    , ("abc", "c", True)
    , ("abc", "ab", True)
    , ("abc", "bc", True)
    , ("abc", "abc", True)
    , ("abc", "xbc", False)
    , ("abc", "abx", False)
    , ("abc", "ax", False)
    , ("abc", "xb", False)
    , ("abc", "xc", False)
    , ("abc", "x", False)
    , ("aaaabaaa", "abaaa", True)
    , ("abcdefghijklmno", "hijkl", True)
    , ("abcdeabcdeabcdef", "abcdef", True)
    ]
