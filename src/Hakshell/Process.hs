-- | Manage system processes.
module Hakshell.Process
  ( Launcher, ProcessEnv(..), execPath,
  ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.State

import qualified Data.ByteString.UTF8  as UTF8
import qualified Data.Vector           as Vec

----------------------------------------------------------------------------------------------------

newtype Launcher a = Launcher (StateT ProcessEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState ProcessEnv Launcher where { state = Launcher . state; }

data ProcessEnv
  = ProcessEnv
    { theExecPath :: !(Vec.Vector UTF8.ByteString)
    }

execPath :: Lens' ProcessEnv [UTF8.ByteString]
execPath = lens (Vec.toList . theExecPath) $ \ a b -> a{ theExecPath = Vec.fromList b }

----------------------------------------------------------------------------------------------------
