module Hakshell.TextEditor.LineBreaker
  ( -- * Line Break Behavior
    --
    -- The line break behavior of the 'TextBuffer' can be programmed to behave differently from the
    -- ordinary default behavior of breaking input strings on the @'\n'@ character.
    LineBreakSymbol(..), lineBreakSize,
    LineBreaker(..), lineBreaker, lineBreakPredicate, defaultLineBreakSymbol,
    lineBreakerNLCR, lineBreakerNL, lineBreakerCR, lineBreakerNUL,
  ) where

import           Control.Arrow ((>>>))
import           Control.Lens

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
    , theLineBreaker :: String -> [(String, LineBreakSymbol)]
      -- ^ This function scans through a string finding character sequences that delimit the end of
      -- a line of text. For each returned tuple, the first element of the tuple should be a string
      -- without line breaks, the second element should contain a string with only line breaks, or
      -- an empty string if the string was not terminated with a line break.
    , theDefaultLineBreakSymbol :: !LineBreakSymbol
      -- ^ This defines the default line break to be used by the line breaking function.
    }

-- | This defines the default line break to be used by the line breaking function.
defaultLineBreakSymbol :: Lens' LineBreaker LineBreakSymbol
defaultLineBreakSymbol = lens theDefaultLineBreakSymbol $ \ a b -> a{ theDefaultLineBreakSymbol = b }

-- | This function is called by 'insertChar' to determine if the 'bufferLineEditor' should be
-- terminated.
lineBreakPredicate :: Lens' LineBreaker (Char -> Bool)
lineBreakPredicate = lens theLineBreakPredicate $ \ a b -> a{ theLineBreakPredicate = b }

-- | This function scans through a string finding character sequences that delimit the end of a line
-- of text.
lineBreaker :: Lens' LineBreaker (String -> [(String, LineBreakSymbol)])
lineBreaker = lens theLineBreaker $ \ a b -> a{ theLineBreaker = b }

-- | This is the default line break function. It will split the line on the character sequence
-- @"\\n"@, or @"\\r"@, or @"\\n\\r"@, or @"\\r\\n"@. The line terminators must be included at the
-- end of each broken string, so that the rule that the law @'Prelude.concat' ('theLineBreaker'
-- 'lineBreakerNLCR' str) == str@ is obeyed.
lineBreakerNLCR :: LineBreaker
lineBreakerNLCR = LineBreaker
  { theLineBreakPredicate     = nlcr
  , theLineBreaker            = lines
  , theDefaultLineBreakSymbol = LineBreakNL
  } where
    nlcr c = c == '\n' || c == '\r'
    lines  = break nlcr >>> \ case
      (""  , "") -> []
      (line, '\n':'\r':more) -> (line, LineBreakNLCR) : lines more
      (line, '\r':'\n':more) -> (line, LineBreakCRNL) : lines more
      (line, c:more)         -> case c of
        '\n' -> (line, LineBreakNLCR) : lines more
        '\r' -> (line, LineBreakCRNL) : lines more
        c    -> error $ "lineBreakerNLCR: 'break' stopped on char "++show c
      (line, "") -> [(line, NoLineBreak)]

-- Not for export
makeLineBreaker :: LineBreakSymbol -> Char -> LineBreaker
makeLineBreaker sym c = LineBreaker
  { theLineBreakPredicate     = (== c)
  , theLineBreaker            = lines
  , theDefaultLineBreakSymbol = LineBreakNL
  } where
  lines = break (== c) >>> \ case
    (""  , ""    ) -> []
    (line, _:more) -> (line, sym) : lines more
    (line, ""    ) -> [(line, NoLineBreak)]

-- | Only breaks lines on @'\n'@ characters, all other characters are treated as ordinary line
-- content.
lineBreakerNL :: LineBreaker
lineBreakerNL = makeLineBreaker LineBreakNL '\n'

-- | Only breaks lines on @'\r'@ characters, all other characters are treated as ordinary line
-- content.
lineBreakerCR :: LineBreaker
lineBreakerCR = makeLineBreaker LineBreakCR '\r'

-- | Only breaks lines on @'\0'@ (null) characters, all other characters are treated as ordinary
-- line content.
lineBreakerNUL :: LineBreaker
lineBreakerNUL = makeLineBreaker LineBreakNUL '\0'
