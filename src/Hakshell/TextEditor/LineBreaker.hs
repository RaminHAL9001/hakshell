module Hakshell.TextEditor.LineBreaker
  ( -- * Line Break Behavior
    --
    -- The line break behavior of the 'TextBuffer' can be programmed to behave differently from the
    -- ordinary default behavior of breaking input strings on the @'\n'@ character.
    LineBreakSymbol(..), lineBreakSize,
    LineBreaker(..), lineBreaker, lineBreakPredicate, defaultLineBreak, lineBreakerNLCR,
  ) where

import           Control.Arrow ((>>>))
import           Control.Lens

import qualified Data.Vector.Unboxed as UVec

import           Hakshell.String

----------------------------------------------------------------------------------------------------

data LineBreakSymbol
  = NoLineBreak
  | LineBreakNL
  | LineBreakCR
  | LineBreakNUL
  | LineBreakNLCR
  | LineBreakCRNL
  deriving (Eq, Ord, Enum)

instance Show LineBreakSymbol where
  show = \ case
    NoLineBreak   -> ""
    LineBreakNL   -> "\n"
    LineBreakCR   -> "\r"
    LineBreakNUL  -> "\0"
    LineBreakNLCR -> "\n\r"
    LineBreakCRNL -> "\r\n"

instance Read LineBreakSymbol where
  readsPrec _ = \ case
    ""            -> [(NoLineBreak, "")]
    '\n':'\r':str -> [(LineBreakNLCR, str)]
    '\r':'\n':str -> [(LineBreakCRNL, str)]
    '\n':str      -> [(LineBreakNL, str)]
    '\r':str      -> [(LineBreakCR, str)]
    '\0':str      -> [(LineBreakNUL, str)]
    _             -> []

lineBreakSize :: Num n => LineBreakSymbol -> n
lineBreakSize = \ case
  NoLineBreak   -> 0
  LineBreakNL   -> 1
  LineBreakCR   -> 1
  LineBreakNUL  -> 1
  LineBreakNLCR -> 2
  LineBreakCRNL -> 2

-- | A pair of functions used to break strings into lines. This function is called every time a
-- string is transferred from 'theBufferLineEditor' to to 'theLinesAbove' or 'theLinesBelow' to ensure
-- all strings entered into a buffer have no more than one line terminating character sequence at
-- the end of them.
--
-- If you choose to use your own 'LineBreaker' function, be sure that the function obeys this law:
--
-- @
-- 'Prelude.concat' ('lineBreakerNLCR' str) == str
-- @
data LineBreaker
  = LineBreaker
    { theLineBreakPredicate :: Char -> Bool
      -- ^ This function is called by 'insertChar' to determine if the 'bufferLineEditor' should be
      -- terminated.
    , theLineBreaker :: String -> [(String, String)]
      -- ^ This function scans through a string finding character sequences that delimit the end of
      -- a line of text. For each returned tuple, the first element of the tuple should be a string
      -- without line breaks, the second element should contain a string with only line breaks, or
      -- an empty string if the string was not terminated with a line break.
    , theDefaultLineBreak :: !CharVector
      -- ^ This defines the default line break to be used by the line breaking function.
    }

-- | This defines the default line break to be used by the line breaking function.
defaultLineBreak :: Lens' LineBreaker CharVector
defaultLineBreak = lens theDefaultLineBreak $ \ a b -> a{ theDefaultLineBreak = b }

-- | This function is called by 'insertChar' to determine if the 'bufferLineEditor' should be
-- terminated.
lineBreakPredicate :: Lens' LineBreaker (Char -> Bool)
lineBreakPredicate = lens theLineBreakPredicate $ \ a b -> a{ theLineBreakPredicate = b }

-- | This function scans through a string finding character sequences that delimit the end of a line
-- of text.
lineBreaker :: Lens' LineBreaker (String -> [(String, String)])
lineBreaker = lens theLineBreaker $ \ a b -> a{ theLineBreaker = b }

-- | This is the default line break function. It will split the line on the character sequence
-- @"\\n"@, or @"\\r"@, or @"\\n\\r"@, or @"\\r\\n"@. The line terminators must be included at the
-- end of each broken string, so that the rule that the law @'Prelude.concat' ('theLineBreaker'
-- 'lineBreakerNLCR' str) == str@ is obeyed.
lineBreakerNLCR :: LineBreaker
lineBreakerNLCR = LineBreaker
  { theLineBreakPredicate = nlcr
  , theLineBreaker = lines
  , theDefaultLineBreak = UVec.fromList "\n"
  } where
    nlcr c = c == '\n' || c == '\r'
    lines  = break nlcr >>> \ case
      (""  , "") -> []
      (line, "") -> [(line, "")]
      (line, '\n':'\r':more) -> (line, "\n\r") : lines more
      (line, '\r':'\n':more) -> (line, "\r\n") : lines more
      (line, c:more)         -> (line, [c])    : lines more
