-- | Combinators for building composable IO functions that share some of the behavior of UNIX
-- pipelines. This is accomplished with a 'Pipe' data type. For the most part, you shouldn't need
-- these functions for day-to-day shell use cases, except perhaps for 'foreach' and 'pmap' which are
-- generally very useful. These combinators are used to define new shell function.
--
-- Hakshell shell functions are of a type similar to this:
--
-- @
-- funcName :: [arguments] -> 'Pipe' IO input -> IO ('Pipe' IO output)
-- @
--
module Hakshell.Pipe where

import           Prelude hiding (fail)

import           Hakshell.String

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.State hiding (fail)

import           Data.Semigroup

import qualified Data.ByteString.Char8 as BStr
import qualified Data.ByteString.UTF8  as UTF8

----------------------------------------------------------------------------------------------------

type ErrMsg = UTF8.ByteString

-- | 'Pipe' is a data type used to approximate the behavior of UNIX pipes.
--
-- You use the 'pull' function to evaluate an 'Applicative' function of your choice on the content
-- within the 'Pipe'. The 'Pipe' is a "Mealy Machine" data type, meaning when 'pull' is evaluated,
-- it pattern match on the 'Pipe' to extract a value to be applied to your 'Applicative' function,
-- and then another applicative function is also extracted and evaluated to produce another 'Pipe',
-- and each successive 'Pipe' is recursively evaluated.
--
-- 'Pipe' instantiates the 'Show' class to show a status:
--
-- * @(OK)@ on the 'PipeNext' constructor, means you can begin extracting values using 'pull'.
--
-- * @(ERROR "some message")@ means an error occurred.
--
-- * An empty string indicates the 'PipeStop' constructor was returned, which means no error
--   ocurred, and no result was returned either.
--
-- Construct a 'Pipe' using functions like 'push' and 'foreach'. Pipe is also a functor, so you can
-- modify the type of @a@ by evaluating a function on it with 'fmap'. 'Pipe' is not an 'Applicative'
-- or a 'Monad', which makes it unlike a list. However like list, it does instantiate 'Semigroup'
-- and 'Monoid', so if you want to sequence items evaluated by 'pull' within a 'Pipe' you can use
-- the ('<>') operator to append more items onto the content stored within the 'Pipe'. For example:
--
-- @
-- return ('push' 1 <> 'push' 2 <> 'push' 3) >>= pull (print . (+ 3))
-- @
--
-- The above will, for each 'push'ed element, add 3 and then print the result.
--
-- Monoid/Semigroup appending is lazy, so you can append a 'PipeFail' value to a chain of
-- non-failing ''Pipe's and evaluate 'pull' such that 'pull' stops evaluating before the failure is
-- reached. This can be dnoe ifting a 'Control.Monad.Cont.ContT' and force the 'pull' evaluator to
-- halt:
--
-- @
-- 'Control.Monad.Cont.runContT'
--     ('Control.Monad.Cont.callCC' $ \\ halt -> -- <- here we define the halt function
--         return ('push' 1 <> 'push' 2 <> PipeFail "error!") >>=
--             'pull' (\\ i -> if i > 1
--                       then halt () -- halt when we get to 2, 'PipeFail' will not be evaluated.
--                       else print (i + 3))
--     ) return -- <-- tell 'ContT' to simply 'return' the last value returned or passed to halt.
-- @
data Pipe m a
  = PipeStop
  | PipeFail !ErrMsg
  | PipeNext !a (m (Pipe m a))

instance Show (Pipe m a) where
  show = \ case
    PipeStop     -> ""
    PipeFail msg -> "(ERROR "++show (unpack msg)++")"
    PipeNext{}   -> "(OK)"

instance Functor m => Functor (Pipe m) where
  fmap f = \ case
    PipeStop        -> PipeStop
    PipeFail   msg  -> PipeFail msg
    PipeNext a next -> PipeNext (f a) $ fmap (fmap f) next

instance Applicative m => Applicative (Pipe m) where
  pure = flip PipeNext (pure PipeStop)
  (<*>) = \ case
    PipeStop         -> const PipeStop
    PipeFail msg     -> const $ PipeFail msg
    PipeNext f nextF -> \ case
      PipeStop         -> PipeStop
      PipeFail msg     -> PipeFail msg
      PipeNext a nextA -> PipeNext (f a) $ (<*>) <$> nextF <*> pure (PipeNext a nextA)

instance Applicative m => Alternative (Pipe m) where
  empty = PipeStop
  (<|>) = \ case
    PipeStop        -> id
    PipeFail msg    -> const $ PipeFail msg
    PipeNext a next -> PipeNext a . (<$> next) . flip (<|>)

instance Monad m => Monad (Pipe m) where
  return = flip PipeNext (return PipeStop)
  (>>=) = \ case
    PipeStop         -> const PipeStop
    PipeFail msg     -> const $ PipeFail msg
    PipeNext a nextA -> \ m -> case m a of
      PipeStop         -> PipeStop
      PipeFail msg     -> PipeFail msg
      PipeNext b nextB -> PipeNext b $ mplus <$> ((>>= m) <$> nextA) <*> nextB

instance Monad m => MonadPlus (Pipe m) where
  mzero = PipeStop
  mplus = \ case
    PipeStop     -> id
    PipeFail msg -> const $ PipeFail msg
    PipeNext a next -> PipeNext a . (<$> next) . flip mplus

instance Monad m => MonadError ErrMsg (Pipe m) where
  throwError = PipeFail
  catchError try catch = case try of
    PipeStop      -> PipeStop
    PipeFail  msg -> catch msg
    ok@PipeNext{} -> ok

instance Monad m => MonadFail (Pipe m) where
  fail = PipeFail . pack

instance (Applicative m, Semigroup a) => Semigroup (Pipe m a) where
  a <> b = (<>) <$> a <*> b

instance (Applicative m, Monoid a) => Monoid (Pipe m a) where
  mempty = pure mempty
  mappend a b = mappend <$> a <*> b

-- | Yield a single value and then end.
push :: Applicative m => a -> m (Pipe m a)
push = pure . flip PipeNext (pure PipeStop)

-- | Yield a pure list of items, each item being 'yield'ed in turn.
pushList :: Applicative m => [a] -> m (Pipe m a)
pushList = \ case
  []   -> pure PipeStop
  a:ax -> pure $ PipeNext a $ pushList ax

-- | Similar to 'forM', but evaluates a function on each element of a list, and each item is
-- 'yield'ed in turn.
pipeEach :: Applicative m => [a] -> (a -> m b) -> m (Pipe m b)
pipeEach ax f = case ax of
  []   -> pure PipeStop
  a:ax -> flip PipeNext (pipeEach ax f) <$> f a

-- | Same as 'foreach' but with the parameters flipped.
mapToPipe :: Applicative m => (a -> m b) -> [a] -> m (Pipe m b)
mapToPipe = flip pipeEach

-- | Like 'pipeEach' but essentially maps a function to each element in the pipe using the ('<*>')
-- operator.
foreach :: Applicative m => Pipe m a -> (a -> m b) -> m (Pipe m b)
foreach ax f = case ax of
  PipeStop        -> pure PipeStop
  PipeFail msg    -> pure $ PipeFail msg
  PipeNext a next -> PipeNext <$> f a <*> (flip foreach f <$> next)

-- | Same as 'foreach' but with the parameters flipped.
pmap :: Applicative m => (a -> m b) -> Pipe m a -> m (Pipe m b)
pmap = flip foreach

-- | Pull values from the 'Pipe', apply each value to the given continuation. The value returned by
-- continuation is ignored, this function is only for evaluating on functions that produce
-- side-effects.
pull :: (MonadIO m, MonadFail m) => (a -> m void) -> Pipe m a -> m ()
pull f = \ case
  PipeStop        -> return ()
  PipeFail   msg  -> Control.Monad.Except.fail $ BStr.unpack msg
  PipeNext a next -> f a >> next >>= pull f

-- | Pull all values from the 'Pipe' until the pipe finishes.
pullList :: (Monad m, MonadFail m) => (a -> m b) -> Pipe m a -> m [b]
pullList f = loop id where
  loop stack = \ case
    PipeStop        -> return $ stack []
    PipeFail   msg  -> Control.Monad.Except.fail $ BStr.unpack msg
    PipeNext a next -> f a >>= \ b -> next >>= loop (stack . (b :))

----------------------------------------------------------------------------------------------------

-- | A 'Control.Monad.State.StateT'-like monad that is used to produce values by unfolding things.
newtype Producer st m a = Producer { unwrapProducer :: StateT st m (Maybe a) }

instance Functor m => Functor (Producer st m) where
  fmap f (Producer g) = Producer $ fmap (fmap f) g

instance Monad m => Monad (Producer st m) where
  return = Producer . return . Just
  (Producer f) >>= m = Producer $ f >>= maybe (return Nothing) (unwrapProducer . m)

instance Monad m => MonadPlus (Producer st m) where
  mzero = Producer $ return Nothing
  mplus (Producer a) (Producer b) = Producer $ a >>= maybe b (return . Just)

instance Monad m => Applicative (Producer st m) where
  pure = Producer . pure . Just
  (Producer f) <*> (Producer a) = Producer $ (<*>) <$> f <*> a

instance Monad m => Alternative (Producer st m) where
  empty = Producer $ return Nothing
  (Producer a) <|> (Producer b) = Producer $ (<|>) <$> a <*> b

-- | Similar to an 'Data.List.unfold', but produces a 'Pipe' of several items. The value to be
-- unfolded can be accessed with the "Control.Monad.State" functions 'Control.Monad.State.get' and
-- 'Control.Monad.State.put'.
produce :: Monad m => st -> Producer st m a -> m (Pipe m a)
produce st p@(Producer f) = runStateT f st >>= \ (next, st) -> return $ case next of
  Nothing -> PipeStop
  Just  a -> PipeNext a $ produce st p

----------------------------------------------------------------------------------------------------

-- | Functions like 'push', 'foreach', and 'pull' are good for simple @IO@ processes. But when your
-- process becomes a little more complicated, it is better to define your @IO@ process in terms of
-- an 'Engine', which manages input, output, an optional state, behind the scenes, allowing you to
-- keep your function definitions clean and compact.
--
-- You define an 'Engine' using the engine combinators such as 'input', 'while', 'collect', and
-- 'output'. You can also use the usual Monad Transformer Library combinators like 'get' and 'put'
-- to update state, the state lenses like 'use' and @('=.')@, error control like 'throwError',
-- 'catchError', and 'Alternative' combinators like @('<|>')@ and 'empty'.
--
-- The name 'Engine' was chosen because it serves as a metaphor for what functions of this type
-- should do: they should take input from a 'Pipe', and produce output to another 'Pipe', typcially
-- performing some work on the content of the input 'Pipe' to produce the output.
newtype Engine st input m a
  = Engine
    { unwrapEngine ::
        StateT (EngineState st input m) m (Pipe (Engine st input m) a)
    }
  deriving (Functor)

data EngineState st input m
  = EngineState
    { theEngineInputPipe  :: !(Pipe m input)
    , theEngineStateValue :: !st
    }

instance Monad m => Applicative (Engine st input m) where
  pure = Engine . pure . flip PipeNext (Engine $ pure PipeStop)
  (Engine f) <*> (Engine a) = Engine $ (<*>) <$> f <*> a

instance Monad m => Alternative (Engine st input m) where
  empty = Engine $ pure empty
  (Engine a) <|> (Engine b) = Engine $ (<|>) <$> a <*> b

instance Monad m => Monad (Engine st input m) where
  return = Engine . return . return
  (Engine a) >>= f = Engine $ do
    let loop = \ case
          PipeStop         -> return $ PipeStop
          PipeFail msg     -> return $ PipeFail msg
          PipeNext a nextA -> unwrapEngine $ mplus
            (Engine $ unwrapEngine $ f a)
            (Engine $ join <$> unwrapEngine nextA >>= loop)
    a >>= loop

instance Monad m => MonadPlus (Engine st input m) where
  mzero = Engine $ return mzero
  mplus (Engine a) (Engine b) = Engine $
    a >>= \ a -> b >>= \ b -> return (mplus a b)

instance MonadTrans (Engine st input) where
  lift = Engine . lift . fmap return

instance Monad m => MonadError ErrMsg (Engine st input m) where
  throwError = Engine . return . PipeFail
  catchError (Engine try) catch = Engine $ try >>= \ case
    PipeStop      -> return PipeStop
    PipeFail  msg -> unwrapEngine $ catch msg
    ok@PipeNext{} -> return ok

instance Monad m => MonadFail (Engine st input m) where
  fail = throwError . pack

instance (Monad m, Semigroup a) => Semigroup (Engine st input m a) where
  a <> b = (<>) <$> a <*> b

instance (Monad m, Monoid a) => Monoid (Engine st input m a) where
  mappend a b = mappend <$> a <*> b
  mempty      = return mempty

-- | You should never need to use this function.
--
-- This function used is internally to define the 'Engine' combinators which take elements from the
-- input pipe. It operates on a value of the concrete 'EngineState' type and not the variable type
-- @st@, meaning you cannot evaluate lens expressions such as 'use' or @('.=')@ or @('%=')@ unless
-- you wrap these expressions in the 'Engine' constructor first. For example the 'putBackInput'
-- function is defined as: @\\ elem -> 'Engine' ('engineInputPipe' '%=' 'PipeNext' elem . 'return')@
--
-- Again, just use the input combinators like 'input', 'collect', 'while', and 'putBackInput'.
engineInputPipe :: Lens' (EngineState st input m) (Pipe m input)
engineInputPipe = lens theEngineInputPipe $ \ a b -> a{ theEngineInputPipe = b }

-- | You should never need to use this function.
--
-- This function is used to define the instnaces of 'get' and 'put' (of the 'MonadState' typeclass)
-- for the 'Engine' function type.
--
-- @
-- 'put' = 'Engine' '.' 'assign' 'engineStateValue'
-- 'get' = Engine '$' 'use' 'engineStateValue'
-- @
engineStateValue :: Lens' (EngineState st input m) st
engineStateValue = lens theEngineStateValue $ \ a b -> a{ theEngineStateValue = b }
