-- | An efficient parser library specifically designed for the "Hakshell.TextEditor". Like
-- AttoParsec, this parser can produce a continuation when reaching the end of an input, allowing
-- you to feed more text into it, and feeding an explicit "end of input" signal to test whether the
-- parser finally succeeds or not.
--
-- The combinators in this module take advantage of the fact that the text being traversed is not in
-- a stream, but already fully buffered in a 'TextBuffer'. This fact has a few implications:
--
-- 1. The position of the parser is an index into a vector, and the parser has O(1) random access to
--    the entire sequence of tokens.
--
-- 2. Backtracking is slightly less expensive than other backtracking parsers because restoring the
--    position of the parser to a prior state is also an O(1) operation.
-- 
-- 3. When inspecting token sequences, the parser need not allocate a copy of the text, it simply
--    needs to denote the start and end indicies of the text within the 'TextBuffer'. This produces
--    considerably less garbage for the memory manager to collect. However a copy of the text in a
--    token sequence can be allocated if requested.
--
-- This parser instantiates the typeclasses provided by Edward Kmett's `parsers` package of abstract
-- parser interfaces, so it is a good idea to write your parser using the combinators from
-- `parsers`, you will consequently be able to use your parser as a MegaParsec parser or an
-- AttoParsec parser, as well as a `Hakehsell.TextEditor.Parser`.
module Hakshell.TextEditor.Parser
  ( Parser, getLocation, getCharCounter,
    getTextLine, getTextLineTags, putTextLineTags, modifyTextLineTags,resetTextLineTags,
    ParStep(..), ParState, newParStateIO,
    module Text.Parser.Char,
    module Text.Parser.Combinators,
    module Text.Parser.Token
  ) where

import           Hakshell.String
import           Hakshell.TextEditor

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.State

import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token

----------------------------------------------------------------------------------------------------

data ParState tags st
  = ParState
    { theBuffer      :: !(TextBuffer tags)
    , theLocation    :: !TextLocation
    , theCharCounter :: !(Relative CharIndex)
    , theLine        :: !(TextLine tags)
    , theUserState   :: st
    }

newParStateIO
  ::
    (Show tags) => --DEBUG
    TextBuffer tags -> st -> IO (Either TextEditError (ParState tags st))
newParStateIO buf ust = runEditTextIO (getLineIndex 1) buf <&> \ case
  Left   err -> Left err
  Right line -> Right $ ParState
    { theBuffer      = buf
    , theLocation    = TextLocation{ theLocationLineIndex = 1, theLocationCharIndex = 1 }
    , theCharCounter = 0
    , theLine        = line
    , theUserState   = ust
    }

-- not for export
--buffer :: Lens' (ParState tags st) (TextBuffer tags)
--buffer = lens theBuffer $ \ a b -> a{ theBuffer = b }

-- not for export
location :: Lens' (ParState tags st) TextLocation
location = lens theLocation $ \ a b -> a{ theLocation = b }

-- not for export
charCounter :: Lens' (ParState tags st) (Relative CharIndex)
charCounter = lens theCharCounter $ \ a b -> a{ theCharCounter = b }

-- not for export
line :: Lens' (ParState tags st) (TextLine tags)
line = lens theLine $ \ a b -> a{ theLine = b }

-- not for export
userState :: Lens' (ParState tags st) st
userState = lens theUserState $ \ a b -> a{ theUserState = b }

----------------------------------------------------------------------------------------------------

data ParStep tags st a
  = Backtrack
  | ParSuccess a
  | ParWaiting !(ParState tags st) (Parser tags st a)
  | ParError   !(ParState tags st) !StrictBytes
  deriving Functor

instance Applicative (ParStep tags st) where
  pure = ParSuccess
  a <*> b = case a of
    Backtrack                 -> Backtrack
    ParSuccess a              -> case b of
      Backtrack                 -> Backtrack
      ParSuccess             b  -> ParSuccess $ a b
      ParWaiting st  (Parser b) -> ParWaiting st $ Parser $ ((ParSuccess a) <*>) <$> b
      ParError   err  msg       -> ParError err msg
    ParWaiting st0 (Parser a) -> ParWaiting st0 $ Parser $ (<*> b) <$> a
    ParError   err  msg       -> ParError err msg

instance Alternative (ParStep tags st) where
  empty = Backtrack
  a <|> b = case a of { Backtrack -> b; a -> a; }

instance Monad (ParStep tags st) where
  return = pure
  (>>=) = \ case
    Backtrack                 -> const Backtrack
    ParSuccess             a  -> ($ a)
    ParWaiting  st (Parser a) -> \ f -> ParWaiting st $ Parser $ (>>= f) <$> a
    ParError    err      msg  -> const $ ParError err msg

instance MonadPlus (ParStep tags st) where { mzero = empty; mplus = (<|>); }

----------------------------------------------------------------------------------------------------

newtype Parser tags st a
  = Parser{ unwrapParser :: StateT (ParState tags st) IO (ParStep tags st a) }
  deriving Functor

instance Applicative (Parser tags st) where
  pure = Parser . return . pure
  (Parser a) <*> (Parser b) = Parser $ (<*>) <$> a <*> b

instance Alternative (Parser tags st) where
  empty = Parser $ return empty
  (Parser a) <|> (Parser b) = Parser $ (<|>) <$> a <*> b

instance Monad (Parser tags st) where
  return = pure
  (Parser a) >>= f = Parser $ a >>= \ case
    Backtrack        -> return Backtrack
    ParSuccess     a -> unwrapParser $ f a
    ParWaiting  st a -> return $ ParWaiting st $ a >>= f
    ParError err msg -> return $ ParError err msg

instance MonadPlus (Parser tags st) where { mzero = empty; mplus = (<|>); }

instance MonadError StrictBytes (Parser tags st) where
  throwError msg = Parser $ flip ParError msg <$> get
  catchError (Parser try) catch = Parser $ try >>= \ case
    Backtrack       -> return Backtrack
    ParSuccess    a -> return $ ParSuccess a
    ParWaiting st a -> return $ ParWaiting st $ catchError a catch
    ParError  _ msg -> unwrapParser $ catch msg

instance MonadFail (Parser tags st) where
  fail = throwError . pack

instance MonadState st (Parser tags st) where
  state f = Parser $ state $ \ st ->
    let (a, ust) = f (st ^. userState) in (ParSuccess a, st & userState .~ ust)

getLocation :: Parser tags st TextLocation
getLocation = Parser $ ParSuccess <$> use location

getCharCounter :: Parser tags st (Relative CharIndex)
getCharCounter = Parser $ ParSuccess <$> use charCounter

getTextLine :: Parser tags st (TextLine tags)
getTextLine = Parser $ ParSuccess <$> use line

getTextLineTags :: Parser tags st tags
getTextLineTags = Parser $ ParSuccess <$> use (line . textLineTags)

putTextLineTags :: tags -> Parser tags st ()
putTextLineTags = Parser . fmap ParSuccess . ((line . textLineTags) .=)

modifyTextLineTags :: (tags -> tags) -> Parser tags st ()
modifyTextLineTags = Parser . fmap ParSuccess . ((line . textLineTags) %=)

resetTextLineTags :: Parser tags st ()
resetTextLineTags = Parser $
  gets theBuffer >>=
  liftIO . runEditTextIO (use bufferDefaultTags) >>= \ case
    Left err -> error $ "(INTERAL ERROR) resetTextLineTags produced error message: "++show err
    Right  a -> fmap ParSuccess . assign (line . textLineTags) $ a

----------------------------------------------------------------------------------------------------

--instance Parsing (Parser tags st) where
--  try f = Parser get >>= \ st -> f <|> (Parser (put st) >> mzero)
