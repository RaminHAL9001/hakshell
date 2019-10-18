-- | Using the operating system to launch processes.
module Hakshell.System
  ( sys
  ) where

import           Hakshell.Pipe
import           Hakshell.String

import           Control.Concurrent        (forkIO, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception         (SomeException(..), throw, catch)

import           System.Exit               (ExitCode(ExitFailure, ExitSuccess))
import           System.IO                 (hGetContents, hPutStr, hFlush, hClose)
import qualified System.Process            as Sys

----------------------------------------------------------------------------------------------------

-- | UTF-8 encoding for all strings going out-to, and coming in from, the operating system. It need
-- not be this way, there are accomodations for switching the string encoding (locale). But let's
-- not do that.
type CmdString = StrictBytes

-- | Same as 'CmdString', the operating system doesn't differentiate between commands and arguments,
-- and neither do we.
type ArgString = StrictBytes

sys
  :: PipeLike pipe
  => CmdString -> [ArgString]
  -> (String -> IO (Pipe a))
  -> pipe String
  -> IO (Pipe a)
sys cmd args cont instream = 
  ( Sys.createProcess $
      (Sys.proc (unpack cmd) (unpack <$> args))
      { Sys.std_in    = Sys.CreatePipe
      , Sys.std_out   = Sys.CreatePipe
      , Sys.std_err   = Sys.Inherit
      , Sys.close_fds = False
      }
  ) >>= \ case
    (Just procIn, Just procOut, Nothing, procHandle) -> do
      mvar <- newEmptyMVar
      forkIO $
        ( (hGetContents procOut >>= fmap Just . cont) `catch`
          (\ (SomeException e) -> putMVar mvar (throw e) >> return Nothing)
        ) >>= maybe (pure ()) (putMVar mvar)
      foreach_ (pipe instream) $ hPutStr procIn
      hFlush procIn
      hClose procIn
      Sys.waitForProcess procHandle >>= \ case
        ExitFailure stat -> error $ show cmd ++ " (exit "++show stat++")"
        ExitSuccess      -> takeMVar mvar
    _ -> error $
      "hakshell internal: 'System.Process.createProcess' produced unexpected result"
