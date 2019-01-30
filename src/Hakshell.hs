-- | This is the "Prelude" of Hakshell, import this and run 'hakshellInitialize' or 'hkini' to begin
-- using it. You can also create a @.ghci@ file which automatically imports 'Hakshell' and runs
-- 'hsini'.
module Hakshell
  ( -- * Before you begin...
    hakshellInitialize, hkini,
    -- * Files and directories
    FSPath(..), FSPathString(..), DirectoryPath(..), FileObject(..),
    directoryPath, ospwd, getOSCurrentDirectory,
  ) where

import           Hakshell.Struct

import           Control.Concurrent
--import           Control.Lens
import           Control.Monad.State

import           Data.Dynamic
import qualified Data.Map               as Map
import qualified Data.IntMap            as IMap
import           Data.String
import qualified Data.Text              as Strict
--import qualified Data.Text.Lazy         as Lazy
--import           Data.Typeable

import qualified System.Directory       as Sys
import           System.FilePath.Posix
import           System.IO.Unsafe
import           System.Posix.Signals   as Sys
import           System.Process

import qualified Foreign.C.Error        as Sys

----------------------------------------------------------------------------------------------------

data HakshellState
  = HakshellState
    { isInitialized        :: Bool
    , theChildProcessTable :: Map.Map Strict.Text (IMap.IntMap ProcessHandle)
      -- ^ Necessary to keep track of which child proccesses are running, especially to control
      -- their input/output and to manage OS events that occur when the child process changes
      -- status, for example when a child process halts itself, or when a child process is waiting
      -- for user input.
    , theSignalHandler     :: Sys.Handler
      -- ^ The handler returned by 'Sys.installHandler'.
    }

-- A default 'HakshellState' value.
hakshellState :: HakshellState
hakshellState = HakshellState
  { isInitialized = False
  , theChildProcessTable = Map.empty
  , theSignalHandler = error "hakshellInitialize has not been evaluated"
  }

hkenv :: MVar HakshellState
hkenv = unsafePerformIO (newMVar hakshellState)

-- | 'hakshellInitialize'
hkini :: IO ()
hkini = hakshellInitialize

-- | This function basically installs a hook into the GHCI process that catches the UNIX @TSTP@
-- signal, that is the "Terminal Stop" signal. This signal is usually sent when a user presses
-- @Ctrl-Z@ on the keyboard. Ordinarily GHCi does not catch @TSTP@ so if you launch an interactive
-- sub-process, for example @Vim@, from within GHCi, then press 'Ctrl-Z' within @Vim@, both @Vim@
-- and GHCi will halt and return control to the GHCi parent process. This is because GHCi was never
-- intended to be used as a shell.
--
-- However by evaluating 'hakshellInitialize', the @TSTP@ signal is caught, so @TSTP@-ing a child
-- process will return control to GHCi.
hakshellInitialize :: IO ()
hakshellInitialize = modifyMVar_ hkenv $ \ st -> if isInitialized st then return st else do
  let hook = CatchInfo $ \ info -> print $ "(SIGNAL "
        ++ show (siginfoSignal info)
        ++ (if siginfoError info == Sys.eOK then "" else ", error")
        ++ ")"
  hookHandle <- Sys.installHandler sigTSTP hook Nothing
  return st{ isInitialized = True, theSignalHandler = hookHandle }

----------------------------------------------------------------------------------------------------

-- | This is a path that is expected to exist, or to be created, but hasn't acutally been selected
-- or created yet.
data FSPath
  = AbsolutePath DirectoryPath
  | RelativePath DirectoryPath
  deriving (Eq, Typeable)

data DirectoryPath
  = EmptyPath
  | DirectoryPath{ targetDirectoryName :: Strict.Text, targetNextChild  :: DirectoryPath }
  | FilePath     { targetFileName      :: Strict.Text, targetFileObject :: FileObject }
  deriving (Eq, Typeable)

data FileObject
  = PlainFile -- ^ A file that has not yet been inspected.
  | BinaryObject{ binaryByteOffset    :: Int}
  | TextFile    { fileLineNumber      :: Int, fileCharColumn :: Int }
  | DataStruct  { targetFileJSONIndex :: StructIndex }
    -- ^ Structured data that is not a 'BinaryObject', for example CSV, JSON, XML, or the text of a
    -- computer programming language.
  deriving (Eq, Typeable)

-- | Construct an 'FSPath' for a directory, and apply a 'DirectoryPath' as the final leaf
-- element. Pass 'EmptyPath' as the 'DirectoryPath' if the path is a directory.
directoryPath :: DirectoryPath -> FSPathString -> FSPath
directoryPath end path' =
  constr $ loop end $ (if rel then id else tail) $ splitDirectories path where
    path   = Strict.unpack $ fsPathStringText path'
    rel    = isRelative path
    constr = if rel then RelativePath else AbsolutePath
    loop end = \ case
      []       -> end
      seg:path -> DirectoryPath
        { targetDirectoryName = Strict.pack seg
        , targetNextChild     = loop end path
        }

----------------------------------------------------------------------------------------------------

newtype FSPathString = FSPathString { fsPathStringText :: Strict.Text }
  deriving (Eq, Ord)
instance IsString FSPathString where { fromString = FSPathString . Strict.pack; }
instance Show FSPathString where { show = show . fsPathStringText; }
instance Read FSPathString where
  readsPrec p = readsPrec p >=> \ (a, rem) -> [(FSPathString a, rem)]

-- | 'getOSCurrentDirectory'
ospwd :: IO FSPath
ospwd = directoryPath EmptyPath <$> getOSCurrentDirectory

-- | The 'cd' command maps to the POSIX @chdir()@ API function. The @chdir()@ function actually
-- modifies the operating system process table entry for the current process, which means when you
-- use @cd@, the update is visible by the @ps@ command, which means that the current working
-- directory is really a function that alters the global state of the operating system.
--
-- This is different behavior from 'pwd' function which maps to 'cur' and 'readCursor'.
getOSCurrentDirectory :: IO FSPathString
getOSCurrentDirectory = fromString <$> Sys.getCurrentDirectory
