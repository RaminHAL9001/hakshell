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
  ( -- * The 'Parser' function type
    Parser, ParserState, newParserState,
    -- TODO: runParserWith, execParserWith, runParser, execParser,
    ParStep(..),

    -- * Primitive Combinators
    --
    -- For the most part, you should avoid using these primitive combinators except to update the
    -- @tags@ value of the current line of text being parsed. For all other purposes, construct a
    -- 'Parser' by way of the API functions defined in the 'Parsing' typeclass, not by use of these
    -- functions directly.

    -- ** Parser state information
    parserGet, parserUse, parserModify, currentTags, parserUserState, thePosition, theCurrentLine,

    -- ** Lifted 'ParserStream' combinators
    --
    -- TODO

    -- * Re-exported modules
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

-- | This is the data type containing the context of all functions of type 'Parser'. Although you
-- cannot inspect or modify this data type directly, you can do so indirectly by using 'parserGet',
-- 'parserUse', or 'parserModify' along with functions like 'thePosition', 'theCurrentLine', or
-- 'currentTags'.
data ParserState tags fold
  = ParserState
    { theParBuffer    :: !(TextBuffer tags)
    , theParStream    :: !(ParserStream tags)
    , theParUserState :: !fold
    }

--parserBuffer :: Lens' (ParserState tags fold) (TextBuffer tags)
--parserBuffer = lens theParBuffer $ \ a b -> a{ theParBuffer = b }

parserStream :: Lens' (ParserState tags fold) (ParserStream tags)
parserStream = lens theParStream $ \ a b -> a{ theParStream = b }

parserUserState :: Lens' (ParserState tags fold) fold
parserUserState = lens theParUserState $ \ a b -> a{ theParUserState = b }

----------------------------------------------------------------------------------------------------

-- | This is the internal control structure for a 'Parser' function type. After evaluating a
-- 'Parser' function, the result of parsing is expressed as a value of this type.
data ParStep tags st a
  = Backtrack
  | ParSuccess a
  | ParWaiting !(ParserState tags st) (Parser tags st a)
  | ParError   !(ParserState tags st) !StrictBytes
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

-- | The most common use of this function type is to define parsers for syntax coloring. This is the
-- 'Parser' function type specificly designed to operate on a 'TextBuffer'. This function type
-- instantiates the 'Parsing' typeclass, so you should define parsers using the 'Parsing' typeclass
-- APIs, and then evaluate these parsers on a 'TextBuffer' to extract information about the text in
-- the 'TextBuffer' using the 'execParser' function.
newtype Parser tags st a
  = Parser{ unwrapParser :: StateT (ParserState tags st) IO (ParStep tags st a) }
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
    let (a, ust) = f (st ^. parserUserState) in (ParSuccess a, st & parserUserState .~ ust)

-- | Construct a new 'ParserState'. Note that this function must be evaluated within an 'EditText'
-- type of function.
newParserState
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => st -> EditText tags m (ParserState tags st)
newParserState ust = do
  buf    <- currentBuffer
  stream <- newParserStream
  return ParserState
    { theParBuffer    = buf
    , theParStream    = stream
    , theParUserState = ust
    }

---- | Evaluates a 'Parser' using an existing 'ParserState' that has already been constructed with a
---- call to 'newParserState'.
--runParserWith
--  :: (MonadIO m
--     , Show tags --DEBUG
--     )
--  => Parser tags fold a -> ParserState tags fold -> EditText tags m (ParStep tags fold a)
--runParserWith (Parser par) st = 

----------------------------------------------------------------------------------------------------

-- | This function takes information from the 'ParserState' internal to the 'Parser' function
-- context. Pass functions like 'thePosition' or 'theCurrentLine' as arguments to this function to
-- obtain information.
parserGet :: (ParserState tags fold -> a) -> Parser tags fold a
parserGet = Parser . fmap ParSuccess . gets

-- | Same as 'parserGet' but takes a 'Lens' parameter instead of a pure function.
parserUse :: Lens' (ParserState tags fold) a -> Parser tags fold a
parserUse = Parser . fmap ParSuccess . use

-- | Like 'modify' but updates part of the 'ParserState' rather than the @fold@ed value.
parserModify :: (ParserState tags fold -> ParserState tags fold) -> Parser tags fold ()
parserModify = Parser . fmap ParSuccess . modify

-- | Pass this function to 'parserGet' to obtain the current position of the parser within the
-- 'TextBuffer'.
thePosition :: ParserState tags fold -> TextLocation
thePosition = parserStreamLocation . theParStream

-- | Pass this function to 'parserGet' to obtain the current line being inspected by the parser.
theCurrentLine :: Parser tags st (TextLine tags)
theCurrentLine = Parser $ ParSuccess . parserStreamCache <$> use parserStream

-- | This is a lens that can be used with 'parserModify' to alter the @tags@' for the current line.
currentTags :: Lens' (ParserState tags fold) tags
currentTags = parserStream . parserStreamTags

----------------------------------------------------------------------------------------------------

--instance Parsing (Parser tags st) where
--  try f = Parser get >>= \ st -> f <|> (Parser (put st) >> mzero)
