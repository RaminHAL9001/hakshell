-- | Manage system processes.
module Hakshell.Process
  ( Launcher,
  ) where

import           Control.Lens
import           Control.Monad.State

import qualified Data.ByteString.Char8 as BStr
import qualified Data.ByteString.UTF8  as UTF8
import qualified Data.Vector           as Vec

import           System.Process

----------------------------------------------------------------------------------------------------

newtype Launcher a = Launcher { unwrapLauncher :: StateT ProcessEnv IO a }
  deriving (Functor, Applicative, Monad, MoandIO)

instance MonadState ProcessEnv Launcher where { state = Launcher . state; }

data ProcessEnv
  = ProcessEnv
    { theExecPath :: !(Vec.Vector UTF8.ByteString)
    }

execPath :: Lens' ProcessEnv [UTF8.ByteString]
execPath = lens (Vec.toList . theExecPath) $ \ a b -> a{ theExecPath = Vec.fromList b }

----------------------------------------------------------------------------------------------------

data Pipe m a
  = PipeStop
  | PipeFail !UTF8.ByteString
  | PipeNext !a (m (Pipe a))

instance Functor m => Functor (Pipe m) where
  fmap f = \ case
    PipeStop        -> PipeStop
    PipeFail   msg  -> PipeFail msg
    PipeNext a next -> PipeNext (f a) (fmap (fmap f) a)

-- | Yield a single value and then end.
push1 :: Applicative m => a -> m (Pipe m a)
push1 = pure . flip PipeNext PipeStop

-- | Yield a pure list of items, each item being 'yield'ed in turn.
push :: Applicative m => [a] -> m (Pipe m a)
push = \ case
  []   -> pure PipeStop
  a:ax -> pure $ PipeNext a $ yieldList ax

-- | Similar to 'forM', but evaluates a function on each element of a list, and each item is
-- 'yield'ed in turn.
class ForEachPipeItem tr where
  foreach :: Applicative m => tr a -> (a -> m b) -> m (Pipe m b)

instance ForEachPipeItem [] where
  foreach = \ case
    []   -> const $ pure PipeStop
    a:ax -> fmap (flip PipeNext $ foreach ax) . ($ a)

instance Applicative m => ForEachPipeItem (Pipe m) where
  foreach = \ case
    PipeStop        -> const $ pure PipeStop
    PipeFail msg    -> const $ pure $ PipeFail msg
    PipeNext a next -> \ f -> flip PipeNext (foreach next f) <$> f a

----------------------------------------------------------------------------------------------------

newtype Producer st m a = Producer { unwrapProducer :: StateT st m (Maybe a) }

instance Functor m => Functor (Producer st m) where
  fmap f (Producer g) = Producer $ fmap (fmap f) g

instance Monad m => Monad (Producer st m) where
  return = Producer . return . Just
  (Producer f) >>= m = Producer $ f >>= maybe (return Nothing) (unwrapProducer . m)

instance Monad m => MonadPlus (Producer st m) where
  mzero = Producer $ return Nothing
  mplus (Producer a) (Producer b) = Producer $ a >>= maybe b (return . Just)

instance Applicative m => Applicative (Producer st m) where
  pure = Producer . pure . Just
  (Producer f) <*> (Producer a) = Producer $ maybe (pure Nothing) (<*> a) <$> f

instance Applicative m => Alternative (Producer st m) where
  empty = Producer $ return Nothing
  (Producer a) <|> (Producer b) = Producer $ maybe b (pure . Just) <$> a

-- | Similar to an 'Data.List.unfold', but produces a 'Pipe' of several items.
produce :: Monad m => st -> Producer st m a -> m (Pipe m a)
produce st (Producer f) = runStateT f st >>= \ (st, next) -> case next of
  Nothing -> return $ PipeStop
  Just  a -> return $ PipeNext a $ generate st f
