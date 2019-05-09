-- | Combinators for building composable IO functions that share some of the behavior of UNIX
-- pipelines. This is accomplished with a 'Pipe' data type. For the most part, you shouldn't need
-- these functions for day-to-day shell use cases. These combinators are used to defining new shell
-- function.
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

-- | 'Pipe' is a Mealy Machine data type, meaning after pattern matching on a 'Pipe' to extract a
-- value, another IO evaluator is also extracted and then evaluated to produce another 'Pipe'.
data Pipe m a
  = PipeStop
  | PipeFail !ErrMsg
  | PipeNext !a (m (Pipe m a))

instance Functor m => Functor (Pipe m) where
  fmap f = \ case
    PipeStop        -> PipeStop
    PipeFail   msg  -> PipeFail msg
    PipeNext a next -> PipeNext (f a) $ fmap (fmap f) next

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
foreach :: Applicative m => [a] -> (a -> m b) -> m (Pipe m b)
foreach ax f = case ax of
  []   -> pure PipeStop
  a:ax -> flip PipeNext (foreach ax f) <$> f a

foreachP :: Applicative m => Pipe m a -> (a -> m b) -> m (Pipe m b)
foreachP ax f = case ax of
  PipeStop        -> pure PipeStop
  PipeFail msg    -> pure $ PipeFail msg
  PipeNext a next -> PipeNext <$> f a <*> (flip foreachP f <$> next)

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
