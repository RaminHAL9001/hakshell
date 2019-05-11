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

import           Control.Applicative
import           Control.Monad.Fail
import           Control.Monad.State hiding (fail)

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

instance Functor m => Functor (Pipe m) where
  fmap f = \ case
    PipeStop        -> PipeStop
    PipeFail   msg  -> PipeFail msg
    PipeNext a next -> PipeNext (f a) $ fmap (fmap f) next

instance Functor m => Monoid (Pipe m a) where
  mempty = PipeStop
  mappend a b = case a of
    PipeNext a nextA -> case b of
      PipeNext{} -> PipeNext a $ flip mappend b <$> nextA
      PipeFail{} -> PipeNext a $ flip mappend b <$> nextA
      PipeStop   -> PipeNext a nextA
    PipeFail{} -> a
    PipeStop   -> b

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
  PipeFail   msg  -> fail $ BStr.unpack msg
  PipeNext a next -> f a >> next >>= pull f

-- | Pull all values from the 'Pipe' until the pipe finishes.
pullList :: (Monad m, MonadFail m) => (a -> m b) -> Pipe m a -> m [b]
pullList f = loop id where
  loop stack = \ case
    PipeStop        -> return $ stack []
    PipeFail   msg  -> fail $ BStr.unpack msg
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
