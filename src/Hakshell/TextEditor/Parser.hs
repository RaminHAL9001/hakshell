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
-- This parser instantiates the typeclasses provided by Edward Kmett's @parsers@ package of abstract
-- parser interfaces, so it is a good idea to write your parser using the combinators from
-- @parsers@, you will consequently be able to use your parser as a MegaParsec parser or an
-- AttoParsec parser, as well as a "Hakehsell.TextEditor.Parser".
module Hakshell.TextEditor.Parser
  ( -- * Parsing over 'TextBuffer's
    --
    -- The parsers in this module are designed specifically to deduce some structure about the text
    -- in a 'TextBuffer'. Usually, this structure is an Abstract Syntax Tree for a programming
    -- language or markup language, and the structure can be used to provide text editing services
    -- like syntax coloring, and smart text search/replace of symbols based on the syntactic type
    -- (like keywords, variable names, and so forth).
    --
    -- Define your 'Parser' using the combinator API functions defined in the @parsers@ library,
    -- Begin by importing the "Text.Parsers.Combinator" module and use any functions in the
    -- 'Parsing' typeclass to define a function of type 'Parser' (the 'Parser' functino type also
    -- instantiates the 'Parsing' typeclass). Then you can run this 'Parser' using one of the
    -- functions defined in this section.

    parseBufferRange, parseBuffer, parseBufferRangeNoTags, parseBufferNoTags,

    -- * The @tags@ type variable
    --
    -- If you started reading documentation in the 'TextBuffer' module you may have noticed that all
    -- text editor combinators take a polymorphic type variable @tags@, as does the 'Parser'
    -- function type itself. The @tags@ variable allows you to define a @data@ structure that can
    -- store useful meta-data about all of the 'TextLine's in a 'TextBuffer', storing information
    -- about various things, such as syntax coloring.
    --
    -- The 'Parser' function type defined in this module, which is evaluated by the above
    -- 'parserTextBuffer' function, can automatically update the @tags@ data structure for your
    -- 'TextBuffer' at the end of every 'TextLine', by way of a 'ParWaiting' data structure (of type
    -- a datum of type 'ParStep'). In order to do this, you must provide a 'Lens' that describes how
    -- to store and retrieve a 'ParStep' data structure within the @data@ structure that you are
    -- using as the @tags@ type.

    TaggedSyntax(..),

    -- * The 'Parser' function type
    Parser, ParserState, newParserState, newParserStateRange, runParser, evalParser,
    ParStep(..),

    -- * Primitive Combinators
    --
    -- For the most part, you should avoid using these primitive combinators except to update the
    -- @tags@ value of the current line of text being parsed. For all other purposes, construct a
    -- 'Parser' by way of the API functions defined in the 'Parsing' typeclass, not by use of these
    -- functions directly.

    -- ** Parser state information
    parserGet, parserUse, parserModify, currentTags, parserUserState, thePosition, theCurrentLine,

    -- ** Primitive combinators
    --
    -- Most of thses functions are simply combinators of type 'StreamCursor' that have been lifted
    -- into the 'Parser' monad to produce new primitive combinators of type 'Parser'.
    parserGoto, parserLook, parserStep, parserCommitTags, parserResetCache, parserResetEndpoint,

    -- ** Pausing/Resuming parsers
    --
    -- For those familiar with the @attoparsec@ library, these primitives provide a similar ability
    -- to pause the 'Parser' and wait for more input, which makes the parser useful in interactive
    -- read-eval-print loops (REPLs).
    --
    -- Unlike @attoparsec@, the Hakshell 'Parser' is designed to operate on a 'TextBuffer', but it
    -- is often useful to store a 'ParWaiting' structure inside of the @tags@ value of every
    -- 'TextLine' within the 'TextBuffer', which allows you to restart a parser after a 'TextLine'
    -- has been edited simply by retrieiving the 'ParserState' stored in the line in the
    -- 'TextBuffer' above the edited line.
    --
    -- This is a very useful feature for text editors with syntax coloring, as it allows for very
    -- rapid re-coloring of syntax due to the fact that the 'Parser' does not need to re-parse the
    -- entire 'TextBuffer' after every edit, only the lines after the edit. And a clever 'Parser'
    -- can even be defined to only parse to the next top-level code construct and then pause itself
    -- again, further increasing the efficiency of the syntax coloring algorithm.

    parserPause_thenDo, parserPause, parserResume, parserResumeAt,

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

-- | Construct a new 'ParserState' which will begin parsing at the first 'TextLocation's and it's
-- EOF is set to the second 'TextLocation'. Note that this function must be evaluated within an
-- 'EditText' type of function.
newParserStateRange
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => TextLocation -> TextLocation -> fold -> EditText tags m (ParserState tags fold)
newParserStateRange start end fold = do
  buf    <- currentBuffer
  stream <- newStreamCursorRange start end
  return ParserState
    { theParBuffer    = buf
    , theParStream    = stream
    , theParUserState = fold
    , theParName      = ""
    }

-- | Similar to 'newParserStateRange', but conveniently passes 'minBound' and 'maxBound' as the
-- start and end points.
newParserState
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => fold -> EditText tags m (ParserState tags fold)
newParserState = newParserStateRange minBound maxBound

-- | This function does nothing for you, apart from simply evaluating a 'Parser' function. You would
-- use this function when defining your own unique 'TextBuffer' analysis algorithms which loop over
-- every 'TextLine' in a 'TextBuffer'. Most of the time, you will probably want to use the
-- 'parseBuffer' family of functions to run a 'Parser' function.
--
-- Evaluates 'newParserState' and then evaluates 'resumeParser' with the new parser state and the
-- given 'Parser' function.
runParser
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => TextLocation -> TextLocation -> Parser tags fold m a -> fold
  -> EditText tags m (ParStep tags fold m a, ParserState tags fold)
runParser start end p = newParserStateRange start end  >=> parserResume p

-- | Similar to 'runParser' but throws away the 'ParserState' and only provides the 'ParStep'
-- result.
evalParser 
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => TextLocation -> TextLocation -> Parser tags fold m a
  -> fold -> EditText tags m (ParStep tags fold m a)
evalParser start end p = fmap fst . runParser start end p

-- not for export
parseBufferRange'
  :: (MonadIO m
     , Show tags
     )
  => TextLocation -> TextLocation -> Maybe (ParserState tags fold -> tags -> tags)
  -> Parser tags fold m a -> fold
  -> EditText tags m (ParStep tags fold m a, fold)
parseBufferRange' start end updateTags par fold = do
  st <- newParserStateRange start end fold
  let start = theStreamLocation $ theParStream st
  parserResume (parserGoto start >> par) st >>= fmap (fmap theParUserState) . loop
  where
    loop result@(status, st) = let s = theParStream st in
      if theStreamLocation s >= theStreamEndpoint s then return result else case status of
        ParWaiting st par ->
          ( flip parserResume st
              (do maybe (pure ()) (\ upd -> parserModify $ currentTags %~ upd st) updateTags
                  parserStep
                  par
              )
          ) >>= loop
        status            -> return (status, st)
{-# INLINE parseBufferRange' #-}

parseBufferRange
  :: (MonadIO m
     , Show tags
     )
  => TextLocation -- ^ The point in the 'TextBuffer' at which to begin parsing
  -> TextLocation -- ^ The point in the 'TextBuffer' at which to end parsing
  -> (ParserState tags fold -> tags -> tags) -- ^ A function to update the @tags@ with a 'ParStep'
  -> Parser tags fold m a -- ^ The parser function to run on the above range.
  -> fold -- ^ An arbitrary state value that can be updated at any time during parsing.
  -> EditText tags m (ParStep tags fold m a, fold)
parseBufferRange start end updateTags = parseBufferRange' start end (Just updateTags)

-- | Calls the 'parserBufferRange' function with 'minBound' and 'maxBound', effectively using the
-- whole buffer as the range of text to parse.
parseBuffer
  :: (MonadIO m
     , Show tags
     )
  => (ParserState tags fold -> tags -> tags)
  -> Parser tags fold m a -> fold
  -> EditText tags m (ParStep tags fold m a, fold)
parseBuffer = parseBufferRange minBound maxBound

-- | Same as 'parseBufferRange' but does not bother updating the @tags@ after parsing each
-- 'TextLine' in the 'TextBuffer'.
parseBufferRangeNoTags
  :: (MonadIO m
     , Show tags
     )
  => TextLocation -- ^ The point in the 'TextBuffer' at which to begin parsing
  -> TextLocation -- ^ The point in the 'TextBuffer' at which to end parsing
  -> Parser tags fold m a -- ^ The parser function to run on the above range.
  -> fold -- ^ An arbitrary state value that can be updated at any time during parsing.
  -> EditText tags m (ParStep tags fold m a, fold)
parseBufferRangeNoTags start end = parseBufferRange' start end Nothing

-- | Same as 'parseBufferRange' but does not bother updating the @tags@ after parsing each
-- 'TextLine' in the 'TextBuffer'.
parseBufferNoTags
  :: (MonadIO m
     , Show tags
     )
  => Parser tags fold m a -- ^ The parser function to run on the above range.
  -> fold -- ^ An arbitrary state value that can be updated at any time during parsing.
  -> EditText tags m (ParStep tags fold m a, fold)
parseBufferNoTags  = parseBufferRangeNoTags minBound maxBound

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
parserLook = do
  -- The order of events here is very important. First check if we are at the end-of-line
  eol <- liftCursorStreamGet (pure . streamIsEOL)
  -- Pause if we are at the end of line.
  when eol parserPause
  -- While paused, the 'parseBuffer' function will evaluate (currentTags %~ updateTags st)
  -- and then 'parserStep' is evaluated. When resuming, finally the next step is evaluated.
  liftCursorStreamState streamLook

-- | Advance the cursor by a single character within the 'TextBuffer', do not read any
-- characters. This function may evaluate 'parserCommitTags' if the cursor advances to the next
-- line. If the cursor does advance to the next line, 'True' is returned, otherwise if the cursor
-- remains on the current line, 'False' is returned.
parserStep 
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Parser tags fold m ()
parserStep = liftCursorStreamModify streamStep

-- | This function pushses the @tags@ value of the current cached 'TextLine'
parserCommitTags
  :: (MonadIO m, TaggedSyntax tags fold
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
parserResetEndpoint = liftCursorStreamModify (streamResetEndpoint maxBound)

----------------------------------------------------------------------------------------------------

-- | Pause the 'Parser', and then evaluate the given continuation function. This function can be
-- used to parse a 'TextBuffer' and pause at the end of each line in the 'TextBuffer'. You can store
-- the current state of the 'Parser' in the @tags@ value of every 'TextLine' in the
-- 'TextBuffer'. When an end user makes a modification to the buffer, you can resume parsing from
-- the 'TextLine' on which the edit occurred.
--
-- Pausing a 'Parser' means dropping back to the calling context in which 'runParser' (or
-- 'evalParser') was called. This does not result in a 'ParError' message, rather it results in a
-- 'ParWaiting' message containing the 'Parser's current state at the time this function was called,
-- and also the continuation function given here, to be returned in the 'ParWaiting' message. You
-- can then evaluate 'runParser' again using the state value and continuation value once your buffer
-- has been updated and is ready to resume parsing. See also: the 'parserPause' function.
parserPause_thenDo
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Parser tags fold m a -> Parser tags fold m a
parserPause_thenDo = Parser . (<$> get) . flip ParWaiting

-- | This function is similar to 'parserWait_thenDo', but is intended to be used more like a
-- single-line instruction in a @do@ block of code. You do not need to pass a continuation function
-- to be used when the 'Parser' is resumed, rather the entire remainder of the @do@ block that is to
-- be executed after this "pause instruction" is treated as the continuation.
parserPause
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Parser tags fold m ()
parserPause = Parser $ flip ParWaiting (pure ()) <$> get

-- | Like 'parserResume' but evaluates 'parserGoto' on the given 'TextLocation' before resuming the
-- given 'Parser'.
parserResumeAt
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Parser tags fold m a -> ParserState tags fold -> TextLocation
  -> EditText tags m (ParStep tags fold m a, ParserState tags fold)
parserResumeAt par st loc = parserResume (parserGoto loc >> par) st

-- | Evaluates a 'Parser' using an existing 'ParserState' that has already been constructed with a
-- call to 'newParserState'.
parserResume
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Parser tags fold m a
  -> ParserState tags fold
  -> EditText tags m (ParStep tags fold m a, ParserState tags fold)
parserResume (Parser par) st = runStateT par st

----------------------------------------------------------------------------------------------------

-- | This class defines a 'Lens' called 'textLineParser' which provides a method of store and
-- retrieve a 'ParStep' result indicating the 'ParserState' of the 'Parser' as it existed when it
-- reached the end of the 'TextLine'. Your @tags@ data type /must/ instantiate this class if you use
-- 'parseTextBuffer'.
--
-- The 'parseTextBufferWith' function provides a method of running a 'Parser' where you supply an
-- arbitrary 'Lens' rather than implicitly passing one by way of the 'Lens' defined in this
-- typeclass for your @tags@ data type, so it is still possible to run a 'Parser' even if your
-- @tags@ have not instantiated this typeclass.
class TaggedSyntax tags fold | tags -> fold where
  textLineParseResult :: MonadIO m => Lens' tags (ParStep tags fold m a)

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
    if streamIsEOF st then return () else mzero <?> "end of input"

----------------------------------------------------------------------------------------------------

instance
  (MonadIO m
  , Show tags --DEBUG
  ) => LookAheadParsing (Parser tags fold m) where

  lookAhead (Parser f) = Parser $ use parserStream >>= (f <*) . assign parserStream

----------------------------------------------------------------------------------------------------

instance
  (MonadIO m
  , Show tags --DEBUG
  ) => CharParsing (Parser tags fold m) where

  satisfy check = parserLook >>= \ c -> if check c then parserStep >> return c else mzero

  anyChar = parserLook <* parserStep

----------------------------------------------------------------------------------------------------

instance
  (MonadIO m
  , Show tags --DEBUG
  ) => TokenParsing (Parser tags fold m) where {} -- All defaults
