-- | Automatic text processing, scripting tools.
module Hakshell.Text
  ( StrictBytes, LazyBytes, ToByteString(..),
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

import           Control.Lens

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
  -- | Match somewhere in the middle of a target string.
  findSubstring :: Capture -> pat -> StrictBytes -> PatternMatch
  -- | Returns the smallest possible string that can be matched by the @pat@ pattern.
  smallestMatch :: pat -> Int

type Label  = StrictBytes
type Offset = Int

data Capture
  = NoMatch            -- ^ if 'StringPattern' matches, evaluate to 'PatternNoMatch'
  | Match              -- ^ if 'StringPattern' matches, evaluate to 'PatternMatch'
  | Capture            -- ^ if 'StringPattern' matches, evaluate to 'PatternMatch' with 'Captured'
  | CaptureName !Label -- ^ like 'Capture' but evaluate to 'PatternMatch' with 'CapturedName'
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
    { sliceStart  :: !Int
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

-- | A string that instantiates the 'StringPattern' class. Use the 'str' constructor.
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
  matchHead capt (ExactString pat) str =
    if Strict.isPrefixOf pat str then case capt of
        NoMatch           -> PatternNoMatch
        Match             -> PatternMatch mempty
        Capture           -> PatternMatch $ Vec.singleton $ Captured 0 pat
        CaptureName label -> PatternMatch $ Vec.singleton $ NamedCaptured label 0 pat
      else case capt of
        NoMatch           -> PatternMatch mempty
        _                 -> PatternNoMatch
