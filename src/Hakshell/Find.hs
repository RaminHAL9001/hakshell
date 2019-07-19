-- | In this module a "find"-like command is defined, and there are a few versions with slightly
-- different behavior: 'find' is the most common, which recursively searches a list of directories
-- piped into it, and the content of each directory is dumped into an output pipe. You can then bind
-- the result additional filters that accept or reject each entry.
module Hakshell.Find
  ( SearchDepth(..), FPath(..),
    FPathList, listFPaths, pathList, pwdPathList,
    FSNode, fsNode,
    FSNodeList, listFSNodes, fsNodeListTime, fsNodeListSource,
    FileMatcher,
    dir, find,
  ) where

import           Hakshell.Pipe
import           Hakshell.String

import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad.State

import           Data.Semigroup
import           Data.String
import           Data.Time.Clock
import qualified Data.Vector            as Vec

import           System.Directory
import           System.Posix.Directory (openDirStream, readDirStream, closeDirStream)
import           System.Posix.Files     (FileStatus, fileID, getFileStatus)
import           System.Posix.Types     (FileID)

----------------------------------------------------------------------------------------------------

-- | There are a few objects which can be used as a path to a file, 'FPath', 'FSNode', and
-- 'FilePath' are all instances of this type class.
class PathToFile path where { toFPath :: path -> FPath; }
instance PathToFile FPath where { toFPath = id; }
instance PathToFile FSNode where { toFPath = fsNodeName; }
instance PathToFile FilePath where { toFPath = pack; }

----------------------------------------------------------------------------------------------------

-- | This is an integer value used to indicate the current search depth.
newtype SearchDepth = SearchDepth{ unwrapSearchDepth :: Int }
  deriving (Eq, Ord, Enum, Num, Bounded)

----------------------------------------------------------------------------------------------------

-- | This is a wrapper around a 'System.IO.FilePath' that has been stored into a 'CharVector'.
newtype FPath = FPath{ fPathName :: CharVector }
  deriving (Eq, Ord)

instance Show FPath where { show = show . fPathName; }
instance IsString FPath where { fromString = FPath . pack; }
instance Packable FPath where { pack = FPath . pack }
instance Unpackable FPath where { unpack (FPath vec) = unpack vec; }

-- | Create an 'FPath' from the current working directory.
pwd :: IO FPath
pwd = FPath . pack <$> getCurrentDirectory

----------------------------------------------------------------------------------------------------

-- | A list of 'FPath's.
newtype FPathList = FPathList{ listFPaths :: [FPath] }
  deriving (Eq, Ord)

-- | Produce a 'FPathList' from the directory given by the 'FPath'. If the 'FPath' refers to a
-- directory, the 'FPathList' will contain the contents of the directory.
pathList :: FPath -> IO FPathList
pathList = (fmap (FPathList . fmap pack) . getDirectoryContents) . unpack . toFPath

-- | Evaluates 'dir' on the current working directory (the result of 'pwd')
pwdPathList :: IO FPathList
pwdPathList = pwd >>= pathList

----------------------------------------------------------------------------------------------------

-- | This data type is created by 'fsNode', and contains dynamic typing information about an 'FPath'
-- obtained from the filesystem. This includes information such as the file type (file, directory,
-- link, socket, etc.), modification time, owner ID, group ID, and permission bits.
data FSNode
  = FSNode
    { fsNodeName :: !FPath
    , fsNodeStat :: !FileStatus
    }

instance Eq FSNode where
  a == b = nn a == nn b && ns a == ns b

instance Ord FSNode where
  compare a b = compare (nn a) (nn b) <> compare (ns a) (ns b)

nn :: FSNode -> FPath
nn = fsNodeName

ns :: FSNode -> FileID
ns = fileID . fsNodeStat

-- | Lookup 'FSNode' information for a given 'FPath'.
fsNode :: FPath -> IO FSNode
fsNode path = FSNode path <$> getFileStatus (unpack path)

----------------------------------------------------------------------------------------------------

data FSNodeList
  = FSNodeList
    { fsNodeListTime   :: UTCTime
      -- ^ The time this data structure was generated.
    , fsNodeListSource :: FPath
      -- ^ The source directory which generated this data structure.
    , listFSNodes     :: [FSNode]
      -- ^ Produce a list of all 'FSNode's in this 'FSNodeList'.
    }
  deriving (Eq, Ord)

-- | Get all 'FSNode's for the given 'FPath', which is usually a directory containing many files.
dirNodes :: FPath -> IO FSNodeList
dirNodes fpath@(FPath path) = do
  start   <- getCurrentTime
  listing <- getDirectoryContents (unpack path) >>= mapM (fsNode . FPath . pack)
  return FSNodeList
    { fsNodeListTime   = start
    , fsNodeListSource = fpath
    , listFSNodes      = listing
    }

-- | Evaluates 'dirNodes' on the current working directory (the result of 'pwd').
pwdNodes :: IO FSNodeList
pwdNodes = pwd >>= dirNodes

----------------------------------------------------------------------------------------------------

-- | Functions of this type scrutinize an 'FSNode', evaluating to 'empty' if the 'FSNode' should not
-- be selected. Functions of this type are evaluated by 'find'.
newtype FileMatcher st a
  = FileMatcher (Engine (FSearchState st) FSNode IO a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance MonadIO (FileMatcher st) where { liftIO = FileMatcher . liftIO; }

instance MonadState st (FileMatcher st) where
  state f = FileMatcher $ state $ \ st ->
    let (a, ust) = f $ st ^. engineStateValue . currentUserState
    in  (a, st & engineStateValue . currentUserState .~ ust)

-- Not for export
data FSearchState st
  = FSearchState
    { theCurrentFSNode      :: FSNode
    , theCurrentFList       :: [FPath]
    , theCurrentSearchDepth :: SearchDepth
    , theCurrentUserState   :: st
    }

currentFSNode :: Lens' (FSearchState st) FSNode
currentFSNode = lens theCurrentFSNode $ \ a b -> a{ theCurrentFSNode = b }

currentFList :: Lens' (FSearchState st) [FPath]
currentFList = lens theCurrentFList $ \ a b -> a{ theCurrentFList = b }

currentSearchDepth :: Lens' (FSearchState st) SearchDepth
currentSearchDepth = lens theCurrentSearchDepth $ \ a b -> a{ theCurrentSearchDepth = b }

currentUserState :: Lens' (FSearchState st) st
currentUserState = lens theCurrentUserState $ \ a b -> a{ theCurrentUserState = b }

-- | Inspect a 'FPath', and if the 'FPath' is a directory, iterate over every 'FSNode's element that
-- exists in the directory as an element of a 'Pipe'. If the given 'FPath' is not a directory, yield
-- a single 'FSNode' in the output 'Pipe'.
dir :: FPath -> IO (Pipe IO FSNode)
dir path = bracket (openDirStream $ unpack path) closeDirStream loop where
  loop stream = do
    name <- readDirStream stream
    if null name then pure PipeStop else do
      node <- fsNode $ pack name
      return $ PipeNext node (loop stream)

find :: Engine (FSearchState ()) FSNode IO a -> FPath -> IO (Pipe IO a)
find = flip foldDir ()

-- | Like 'find' but allows you to construct a stateful value as the find proceeds.
foldDir
  :: Engine (FSearchState st) FSNode IO a
  -> st -> FPath -> IO (Pipe IO a)
foldDir f st = dir >=> step init where
  loop = do
    error "TODO: foldDir"
  init node next = evalEngine loop EngineState
    { theEngineInputPipe  = next
    , theEngineStateValue = FSearchState
        { theCurrentFSNode      = node
        , theCurrentFList       = []
        , theCurrentSearchDepth = 0
        , theCurrentUserState   = st
        }
    }

-- "Fast" means subdirectories are not kept open when recursively searching, which can lead to race
-- conditions if other processes are updating the filesystem within the subtree being searched.
fastSearch
  :: MonadIO m
  => Engine (FSearchState ()) FPath m FSNode
fastSearch = error "TODO: fastSearch"

