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
    runParserWith, runParser, evalParser,
    ParStep(..),

    -- * Primitive Combinators
    --
    -- For the most part, you should avoid using these primitive combinators except to update the
    -- @tags@ value of the current line of text being parsed. For all other purposes, construct a
    -- 'Parser' by way of the API functions defined in the 'Parsing' typeclass, not by use of these
    -- functions directly.

    -- ** Parser state information
    parserGet, parserUse, parserModify, currentTags, parserUserState, thePosition, theCurrentLine,

    -- ** Lifted 'StreamCursor' combinators
    parserGoto, parserLook, parserStep, parserCommitTags, parserResetCache, parserResetEndpoint,

    -- *** Parser state utilities
    --
    -- These functions shouldn't be necessary unless you have some very odd function that alters the
    -- 'StreamCursror' in an 'EditText' function context that cannot be done with any of the other
    -- combinators provided in this module.
    liftCursorStreamState, liftCursorStreamModify, liftCursorStreamGet,

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
import           Text.Parser.LookAhead
import           Text.Parser.Token

----------------------------------------------------------------------------------------------------

-- | This is the data type containing the context of all functions of type 'Parser'. Although you
-- cannot inspect or modify this data type directly, you can do so indirectly by using 'parserGet',
-- 'parserUse', or 'parserModify' along with functions like 'thePosition', 'theCurrentLine', or
-- 'currentTags'.
data ParserState tags fold
  = ParserState
    { theParBuffer    :: !(TextBuffer tags)
    , theParStream    :: !(StreamCursor tags)
    , theParName      :: !StrictBytes
    , theParUserState :: !fold
    }

--parserBuffer :: Lens' (ParserState tags fold) (TextBuffer tags)
--parserBuffer = lens theParBuffer $ \ a b -> a{ theParBuffer = b }

parserStream :: Lens' (ParserState tags fold) (StreamCursor tags)
parserStream = lens theParStream $ \ a b -> a{ theParStream = b }

parserUserState :: Lens' (ParserState tags fold) fold
parserUserState = lens theParUserState $ \ a b -> a{ theParUserState = b }

parserName :: Lens' (ParserState tags fold) StrictBytes
parserName = lens theParName $ \ a b -> a{ theParName = b }

----------------------------------------------------------------------------------------------------

-- | This is the internal control structure for a 'Parser' function type. After evaluating a
-- 'Parser' function, the result of parsing is expressed as a value of this type.
data ParStep tags fold m a
  = Backtrack
  | ParSuccess a
  | ParWaiting !(ParserState tags fold) (Parser tags fold m a)
  | ParError   !(ParserState tags fold) !StrictBytes
  deriving Functor

instance Monad m => Applicative (ParStep tags fold m) where
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

instance Monad m => Alternative (ParStep tags fold m) where
  empty = Backtrack
  a <|> b = case a of { Backtrack -> b; a -> a; }

instance Monad m => Monad (ParStep tags fold m) where
  return = pure
  (>>=) = \ case
    Backtrack                 -> const Backtrack
    ParSuccess             a  -> ($ a)
    ParWaiting  st (Parser a) -> \ f -> ParWaiting st $ Parser $ (>>= f) <$> a
    ParError    err      msg  -> const $ ParError err msg

instance Monad m => MonadPlus (ParStep tags fold m) where { mzero = empty; mplus = (<|>); }

----------------------------------------------------------------------------------------------------

-- | The most common use of this function type is to define parsers for syntax coloring. This is the
-- 'Parser' function type specificly designed to operate on a 'TextBuffer'. This function type
-- instantiates the 'Parsing' typeclass, so you should define parsers using the 'Parsing' typeclass
-- APIs, and then evaluate these parsers on a 'TextBuffer' to extract information about the text in
-- the 'TextBuffer' using the 'execParser' function.
newtype Parser tags fold m a
  = Parser
    { unwrapParser :: StateT (ParserState tags fold) (EditText tags m) (ParStep tags fold m a)
    }
  deriving Functor

instance Monad m => Applicative (Parser tags fold m) where
  pure = Parser . return . pure
  (Parser a) <*> (Parser b) = Parser $ (<*>) <$> a <*> b

instance Monad m => Alternative (Parser tags fold m) where
  empty = Parser $ return empty
  (Parser a) <|> (Parser b) = Parser $ (<|>) <$> a <*> b

instance Monad m => Monad (Parser tags fold m) where
  return = pure
  (Parser a) >>= f = Parser $ a >>= \ case
    Backtrack        -> return Backtrack
    ParSuccess     a -> unwrapParser $ f a
    ParWaiting  st a -> return $ ParWaiting st $ a >>= f
    ParError err msg -> return $ ParError err msg

instance Monad m => MonadPlus (Parser tags fold m) where { mzero = empty; mplus = (<|>); }

instance Monad m => MonadError StrictBytes (Parser tags fold m) where
  throwError msg = Parser $ flip ParError msg <$> get
  catchError (Parser try) catch = Parser $ try >>= \ case
    Backtrack       -> return Backtrack
    ParSuccess    a -> return $ ParSuccess a
    ParWaiting st a -> return $ ParWaiting st $ catchError a catch
    ParError  _ msg -> unwrapParser $ catch msg

instance Monad m => MonadFail (Parser tags fold m) where
  fail = throwError . pack

instance Monad m => MonadState fold (Parser tags fold m) where
  state f = Parser $ state $ \ st ->
    let (a, ust) = f (st ^. parserUserState) in (ParSuccess a, st & parserUserState .~ ust)

-- | Construct a new 'ParserState'. Note that this function must be evaluated within an 'EditText'
-- type of function.
newParserState
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => fold -> EditText tags m (ParserState tags fold)
newParserState fold = do
  buf    <- currentBuffer
  stream <- newStreamCursor
  return ParserState
    { theParBuffer    = buf
    , theParStream    = stream
    , theParUserState = fold
    , theParName      = ""
    }

-- | Evaluates a 'Parser' using an existing 'ParserState' that has already been constructed with a
-- call to 'newParserState'.
runParserWith
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Parser tags fold m a
  -> ParserState tags fold
  -> EditText tags m (ParStep tags fold m a, ParserState tags fold)
runParserWith (Parser par) st = runStateT par st

-- | Similar to 'runParserWith', but evaluates 'newParserState' before running the parser.
runParser
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Parser tags fold m a -> fold
  -> EditText tags m (ParStep tags fold m a, ParserState tags fold)
runParser p = newParserState >=> runParserWith p

-- | The simplest way to evaluate a 'Parser', this function creates a new 'ParserState', throws away
-- the 'ParserState' when completed, and only returns the 'ParStep' result.
evalParser 
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Parser tags fold m a -> fold -> EditText tags m (ParStep tags fold m a)
evalParser p = fmap fst . runParser p

----------------------------------------------------------------------------------------------------

-- | This function takes information from the 'ParserState' internal to the 'Parser' function
-- context. Pass functions like 'thePosition' or 'theCurrentLine' as arguments to this function to
-- obtain information.
parserGet :: Monad m => (ParserState tags fold -> a) -> Parser tags fold m a
parserGet = Parser . fmap ParSuccess . gets

-- | Same as 'parserGet' but takes a 'Lens' parameter instead of a pure function.
parserUse :: Monad m => Lens' (ParserState tags fold) a -> Parser tags fold m a
parserUse = Parser . fmap ParSuccess . use

-- | Like 'modify' but updates part of the 'ParserState' rather than the @fold@ed value.
parserModify
  :: Monad m
  => (ParserState tags fold -> ParserState tags fold) -> Parser tags fold m ()
parserModify = Parser . fmap ParSuccess . modify

-- | Pass this function to 'parserGet' to obtain the current position of the parser within the
-- 'TextBuffer'.
thePosition :: ParserState tags fold -> TextLocation
thePosition = theStreamLocation . theParStream

-- | Pass this function to 'parserGet' to obtain the current line being inspected by the parser.
theCurrentLine :: Monad m => Parser tags fold m (TextLine tags)
theCurrentLine = Parser $ ParSuccess . theStreamCache <$> use parserStream

-- | This is a lens that can be used with 'parserModify' to alter the @tags@' for the current line.
currentTags :: Lens' (ParserState tags fold) tags
currentTags = parserStream . streamTags

----------------------------------------------------------------------------------------------------

-- | Lift a function that modifies a 'StreamCursor' within the lifted 'EditText' function context.
liftCursorStreamState
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => (StreamCursor tags -> EditText tags m (a, StreamCursor tags))
  -> Parser tags fold m a
liftCursorStreamState f = Parser $ do
  (a, fold) <- use parserStream >>= lift . f
  parserStream .= fold
  return (ParSuccess a)

-- | Like 'liftCursorStreamState', but the lifted function only modifies the 'StreamCursor' and
-- doesn't return any other value.
liftCursorStreamModify
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => (StreamCursor tags -> EditText tags m (StreamCursor tags))
  -> Parser tags fold m ()
liftCursorStreamModify f = Parser $ use parserStream >>= lift . f >>=
  fmap ParSuccess . assign parserStream

-- | Like 'liftCursorStreamGet', but the lifted function only modifies the 'StreamCursor' and
-- doesn't return any other value.
liftCursorStreamGet
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => (StreamCursor tags -> EditText tags m a)
  -> Parser tags fold m a
liftCursorStreamGet f = Parser $ use parserStream >>= fmap ParSuccess . lift . f

-- | Change the current position of the parser's cursor. This function lifts 'streamGoto'.
parserGoto
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => TextLocation -> Parser tags fold m ()
parserGoto = (>>) parserResetCache .
  liftCursorStreamModify . flip streamGoto

-- | Get the character under the cursor without advancing the cursor. This function lifts
-- 'streamLook'.
parserLook 
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Parser tags fold m Char
parserLook = liftCursorStreamGet streamLook

-- | Get the character under the cursor and advances the cursor. This function lifts 'streamStep'.
parserStep 
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Parser tags fold m Char
parserStep = liftCursorStreamState streamStep

-- | This function pushses the @tags@ value of the current cached 'TextLine'
parserCommitTags
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Parser tags fold m ()
parserCommitTags = liftCursorStreamGet streamCommitTags

-- | This function ensures that the cached 'TextLine' being analyzed by the parser is the same
-- 'TextLine' that is under the parsing cursor. This function is called automatically by
-- 'parserGoto'.
parserResetCache
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Parser tags fold m ()
parserResetCache = liftCursorStreamModify streamResetCache

-- | This function must be called if the 'TextBuffer' over which a parser is running is modified
-- while the parser is paused.
parserResetEndpoint
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Parser tags fold m ()
parserResetEndpoint = liftCursorStreamModify streamResetEndpoint

----------------------------------------------------------------------------------------------------

instance
  (MonadIO m
  , Show tags --DEBUG
  ) => Parsing (Parser tags fold m) where

  try f = parserGet id >>= \ st -> f <|> (parserModify (const st) >> mzero)

  (<?>) (Parser f) newName = Parser $ do
    oldName <- use parserName
    result  <- f
    parserName .= oldName
    case result of
      Backtrack          -> get >>= \ st -> return $ ParError st $ "expecting: " <> pack newName
      ParWaiting st cont -> return $ ParWaiting st $ cont <?> newName
      _                  -> return result

  unexpected = Control.Monad.Fail.fail

  notFollowedBy (Parser f) = do
    loc <- parserGet thePosition
    result <- Parser (ParSuccess <$> f)
    let success = parserGoto loc
    case result of
      Backtrack{}        -> success
      ParError{}         -> success
      ParSuccess{}       -> mzero
      ParWaiting st cont -> Parser $ return $ ParWaiting st $ notFollowedBy cont

  eof = Parser (ParSuccess <$> use parserStream) >>= \ st ->
    if theStreamLocation st == theStreamEndpoint st then return () else mzero <?> "end of input"

----------------------------------------------------------------------------------------------------

instance
  (MonadIO m
  , Show tags --DEBUG
  ) => LookAheadParsing (Parser tags fold m) where

  lookAhead (Parser f) = Parser $ use parserStream >>= (f <*) . assign parserStream
