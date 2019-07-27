-- | In this module a "find"-like command is defined, and there are a few versions with slightly
-- different behavior: 'find' is the most common, which recursively searches a list of directories
-- piped into it, and the content of each directory is dumped into an output pipe. You can then bind
-- the result additional filters that accept or reject each entry.
module Hakshell.Find
  ( -- * Executing a Filesystem Query

    search, fastSearch, lsr,

    -- * The 'FPath' datatype
    --
    -- A string-like data type for representing filesystem path values.

    FPath(..), pwd,

    -- ** 'FPathList'
    --
    -- A collection of 'FPath' values. These functions read the content of an entire directory into
    -- memory in a single operation.

    FPathList, listFPaths, fPathList, pwdPathList,

    -- * The 'FSNode' datatype
    --
    -- Represents an entry in a filesystem, not just it's path. This includes information about the
    -- file type, it's owners, permission settings, and the size of the file.

    FSNode, fsNode,

    -- ** 'FSNodeList'
    --
    -- A collection of 'FSNode' values. These functions read the content of an entire directory into
    -- memory in a single operation.

    FSNodeList, fsNodeList, fsNodeListTime, fsNodeListSource,
    listFSNodes, pwdListFSNode,

    -- * Predicates on 'FSNode' values

    FSNodeTest(..),
    fnot, eitherFSNodeTest, anyFSNodeTest,

    -- ** Predicates for 'search'
    --
    -- These predicates are specifically designed for use with the 'search' function.

    file, dir, isNamed, socket, link, fifo,

    -- *** Lower-level Predicates
    --
    -- These predicates simply take information from a 'FSearchState', allowing you to define your
    -- own predicates on the values in this data structure, if you really need to

    fsNodePredicate, fNamePredicate,

    -- * Querying the Filesystem

    FileMatchResult(..), prune, noMatch, matchPrune, match,

    -- *** Lenses on 'FileMatchResult's.

    fileMatchStepInto, fileMatchYield, fileMatchResult,

    -- ** Deciding which files match
    --
    -- Functionality for writing predicates on files that can be used during a filesystem search.

    FileMatcher, ftest, (?->), 

    -- *** Low-level access to the state of a query
    --
    -- An 'FSearchState' is created by the 'search' function and can be accessed from within a
    -- 'FileMatcher' function via the 'Control.Monad.Reader.ask' function of the
    -- "Control.Monad.Reader" API. Ususally you will not need to use the 'FSearchState' data type
    -- directly, rather you will evaluate some of the pre-defined predicates like 'file' or 'dir'
    -- using the 'ftest' function or the @('?->')@ operator.

    FSearchState, theCurrentFSNode,
    theCurrentSearchDepth, SearchDepth(..),
    theCurrentSearchPath, theCurrentUserState,

  ) where

import           Hakshell.Pipe
import           Hakshell.String

import           Control.Applicative
import           Control.Arrow
import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe

import           Data.Semigroup
import           Data.String
import           Data.Time.Clock

import           System.Directory
                 ( getCurrentDirectory
                 , getDirectoryContents
                 )
import           System.FilePath( (</>) )
import           System.Posix.Directory
                 ( openDirStream, readDirStream, closeDirStream
                 )
import           System.Posix.Files
                 ( FileStatus, fileID, getFileStatus
                 , isDirectory, isNamedPipe, isSocket, isSymbolicLink, isRegularFile,
                 )
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
fPathList :: FPath -> IO FPathList
fPathList = (fmap (FPathList . fmap pack) . getDirectoryContents) . unpack . toFPath

-- | Evaluates 'fPathList' on the current working directory (the result of 'pwd')
pwdPathList :: IO FPathList
pwdPathList = pwd >>= fPathList

----------------------------------------------------------------------------------------------------

-- | This data type is created by 'fsNode', and contains dynamic typing information about an 'FPath'
-- obtained from the filesystem. This includes information such as the file type (file, directory,
-- link, socket, etc.), modification time, owner ID, group ID, and permission bits.
data FSNode
  = FSNode
    { fsNodeParent :: !FPath
    , fsNodeName   :: !FPath
    , fsNodeStat   :: !FileStatus
    }

instance Eq FSNode where
  a == b = nn a == nn b && ns a == ns b

instance Ord FSNode where
  compare a b = compare (np a) (np b) <> compare (nn a) (nn b) <> compare (ns a) (ns b)

instance Show FSNode where { show n = unpack (np n) </> unpack (nn n); }

np :: FSNode -> FPath
np = fsNodeParent

nn :: FSNode -> FPath
nn = fsNodeName

ns :: FSNode -> FileID
ns = fileID . fsNodeStat

-- | Lookup 'FSNode' information for a given file 'FPath' within a given directory 'FPath'. The
-- directory and file are concatenated with a directory separator @('</>')@ and this concatenated
-- path is used to query the operating system for the 'FileStatus'.
fsNode :: FPath -> FPath -> IO FSNode
fsNode parent file = FSNode parent file <$> getFileStatus (unpack parent </> unpack file)

-- | Get the full path for an 'FSNode', which includes the 'fsNodeParent' concatenated with the
-- 'fsNodeName' using the path concatenation operator @('</>')@.
fsNodePath :: FSNode -> FPath
fsNodePath p = pack $ unpack (np p) </> unpack (nn p)

----------------------------------------------------------------------------------------------------

data FSNodeList
  = FSNodeList
    { fsNodeListTime   :: UTCTime
      -- ^ The time this data structure was generated.
    , fsNodeListSource :: FPath
      -- ^ The source directory which generated this data structure.
    , fsNodeListNodes  :: [FSNode]
      -- ^ Produce a list of all 'FSNode's in this 'FSNodeList'.
    }
  deriving (Eq, Ord)

-- | Get all 'FSNode's for the given 'FPath', which is usually a directory containing many files.
fsNodeList :: FPath -> IO FSNodeList
fsNodeList dir = do
  start   <- getCurrentTime
  listing <- getDirectoryContents (unpack dir) >>= mapM (fsNode dir . FPath . pack)
  return FSNodeList
    { fsNodeListTime   = start
    , fsNodeListSource = dir
    , fsNodeListNodes  = listing
    }

-- | Evaluates 'fsNodeList' on the current working directory (the result of 'pwd').
pwdListFSNode :: IO FSNodeList
pwdListFSNode = pwd >>= fsNodeList

----------------------------------------------------------------------------------------------------

-- | This function type is used to test properties on a 'FSNode'. It is defined to instantiate the
-- 'Data.Monoid.Monoid' typeclass such that two 'FSNodeTests' @a@ and @b@ joined together with the
-- @('Data.Semigroup.<>')@ operator like so: @(a <> b)@ is equivalent to evaluating the logical
-- conjunction (a AND b must both be true). The @('Data.Semigroup.<>')@ operator is defined as:
--
-- @
-- (<>) a b = FSNodeText (\\ file -> 'runFSNodeTest' a file 'Prelude.&&' 'runFSNodeTest' b file)
-- @
--
-- To perform a negation (logical NOT) use 'fnot'.
--
-- To perform a disjunction (logical OR) of 'FSNodeTest's, use 'eitherFSNodeTest' or
-- 'anyFSNodeTest', however the 'FileMatcher' functions 'ftest' and the operator @'?->'@ takes a
-- list of 'FSNodeTest' functions and performs a logical disjunction anyway, so usually the
-- 'eitherFSNodeTest' function is not necessary, just construct a list of 'FSNodeTest's.
newtype FSNodeTest st
  = FSNodeTest
    { runFSNodeTest :: FSearchState st -> Bool
      -- ^ Evaluate an 'FSNodeTest' function on an 'FSearchState' value.
    }

instance Semigroup (FSNodeTest st) where
  a <> b = FSNodeTest $ \ file -> runFSNodeTest a file && runFSNodeTest b file

instance Monoid (FSNodeTest st) where
  mempty = FSNodeTest $ const True
  mappend = (<>)

fnot :: FSNodeTest st -> FSNodeTest st
fnot (FSNodeTest test) = FSNodeTest $ not . test

-- | Perform a disjunction (logical OR) of 'FSNodeTest's, use 'eitherFSNodeTest' or
-- 'anyFSNodeTest'. To perform a negation (logical NOT) use 'fnot'
eitherFSNodeTest :: FSNodeTest st -> FSNodeTest st -> FSNodeTest st
eitherFSNodeTest a b = FSNodeTest $ \ file -> runFSNodeTest a file || runFSNodeTest b file

-- | Fold 'eitherFSNodeTest' on a list of 'FSNodeTest's, this is the 'FSNodeTest' analogue of the
-- 'Prelude.or' function.
anyFSNodeTest :: [FSNodeTest st] -> FSNodeTest st
anyFSNodeTest = \ case { [] -> FSNodeTest $ const False; a:ax -> foldl eitherFSNodeTest a ax }

-- | This function evaluates any predicate on the 'theCurrentFSNode' while evaluating a
-- 'FileMatcher' function.
fsNodePredicate :: (FSNode -> Bool) -> FSNodeTest st
fsNodePredicate = FSNodeTest . (. theCurrentFSNode)

-- | This function converts a predicate on a 'System.Posix.Files.FileStatus' value and converts it
-- to a 'FSNodeTest' predicate. This means you pass a predicate function from the
-- "System.Posix.Files" module, and produce a 'FSNodeTest' function that evaluates the predicate on
-- the 'fsNodeStat' property of an 'FSNode' data type.
fsPredicate :: (FileStatus -> Bool) -> FSNodeTest st
fsPredicate = fsNodePredicate . (. fsNodeStat)

-- | Matches directories
dir :: FSNodeTest st
dir = fsPredicate isDirectory

-- | Matches regular files
file :: FSNodeTest st
file = fsPredicate isRegularFile

-- | Matches UNIX sockets
socket :: FSNodeTest st
socket = fsPredicate isSocket

-- | Matches UNIX named pipes, also sometimes called "FIFOs".
fifo :: FSNodeTest st
fifo = fsPredicate isNamedPipe

-- | Matches symbolic links
link :: FSNodeTest st
link = fsPredicate isSymbolicLink

-- | Evaluates a predicate on the 'fsNodeName' of an 'FSNode'.
fNamePredicate :: (FPath -> Bool) -> FSNodeTest st
fNamePredicate = fsNodePredicate . (. fsNodeName)

-- | File name (the basename) matches the exact string given:
isNamed :: FPath -> FSNodeTest st
isNamed = fNamePredicate . (==)

----------------------------------------------------------------------------------------------------

-- | A 'FileMatcher' must return a 'FileMatch' value, which is used by the 'search' function to
-- control the behavior of the query after a file matches or does not match.
--
-- The 'ftest' function
data FileMatchResult a
  = FileMatchResult
    { theFileMatchStepInto :: !Bool
      -- ^ If this file match is scrutinizing a directory, the 'search' function must decide whether
      -- to step into this directory (depth first), set this value to tell 'search' whether or not
      -- to do so. Setting this value to 'False' will prune the directory under scrutiny from
      -- recursively being 'search'ed.
    , theFileMatchYield    :: [a]
      -- ^ A file match result may yield zero or more values.
    }
  deriving (Eq, Functor)

-- | This function is used within a 'FileMatcher' function. Do not output a value, do not step into
-- this subdirectory if the current match is scrutinizing a subdirectory,
prune :: Applicative m => m (FileMatchResult a)
prune = pure $ FileMatchResult False []

-- | This function is used within a 'FileMatcher' function. Do not output any value, but do step
-- into this subdirectory if the current match is scrutinizing a subdirectory. This is the default
-- behavior for the 'ftest' function.
noMatch :: Applicative m => m (FileMatchResult a)
noMatch = pure $ FileMatchResult True []

-- | Output the file being scrutinized, but do not step into this subdirectory if the current match
-- is scrutinizing a subdirectory.
matchPrune :: FileMatcher st (FileMatchResult FSNode)
matchPrune = FileMatchResult False . pure <$> asks theCurrentFSNode

-- | Output the file being scrutinized, and do step into this subdirectory if the current match is
-- scrutinizing a subdirectory.
match :: FileMatcher st (FileMatchResult FSNode)
match = FileMatchResult True . pure <$> asks theCurrentFSNode

-- | If this file match is scrutinizing a directory, the 'search' function must decide whether to
-- step into this directory (depth first), set this value to tell 'search' whether or not to do
-- so. Setting this value to 'False' will prune the directory under scrutiny from recursively being
-- 'search'ed.
fileMatchStepInto :: Lens' (FileMatchResult a) Bool
fileMatchStepInto = lens theFileMatchStepInto $ \ a b -> a{ theFileMatchStepInto = b }

fileMatchYield :: Lens' (FileMatchResult a) [a]
fileMatchYield = lens theFileMatchYield $ \ a b -> a{ theFileMatchYield = b }

-- | Construct a default 'FileMatch' value, the default behavior is to set the 'fileMatchStepInto'
-- value to 'True'.
fileMatchResult :: [a] -> FileMatchResult a
fileMatchResult = FileMatchResult True

----------------------------------------------------------------------------------------------------

-- | Functions of this type scrutinize an 'FSNode', evaluating to 'empty' if the 'FSNode' should not
-- be selected. Functions of this type are evaluated by 'find'.
newtype FileMatcher st a
  = FileMatcher (MaybeT (StateT (FSearchState st) IO) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO)

instance MonadState st (FileMatcher st) where
  state f = FileMatcher $ state $ \ st ->
    let (a, ust) = f $ st ^. currentUserState
    in  (a, st & currentUserState .~ ust)

instance MonadReader (FSearchState st) (FileMatcher st) where
  ask = FileMatcher $ lift get
  local step (FileMatcher f) = FileMatcher $ (>>= (maybe empty pure)) $ lift $ do
    oldst <- state $ fmap (const ()) &&& step
    runMaybeT f <* modify (\ newst -> const (theCurrentUserState newst) <$> oldst)

-- | This function contains the current state of a 'search' operation.
data FSearchState st
  = FSearchState
    { theCurrentFSNode      :: FSNode
    , theCurrentSearchPath  :: [FPath]
    , theCurrentSearchDepth :: SearchDepth
    , theCurrentUserState   :: st
    }
  deriving Functor

currentFSNode :: Lens' (FSearchState st) FSNode
currentFSNode = lens theCurrentFSNode $ \ a b -> a{ theCurrentFSNode = b }

currentSearchPath :: Lens' (FSearchState st) [FPath]
currentSearchPath = lens theCurrentSearchPath $ \ a b -> a{ theCurrentSearchPath = b }

currentSearchDepth :: Lens' (FSearchState st) SearchDepth
currentSearchDepth = lens theCurrentSearchDepth $ \ a b -> a{ theCurrentSearchDepth = b }

currentUserState :: Lens' (FSearchState st) st
currentUserState = lens theCurrentUserState $ \ a b -> a{ theCurrentUserState = b }

-- not for export
runFileMatcher :: FileMatcher st a -> FSearchState st -> IO (Maybe a, FSearchState st)
runFileMatcher (FileMatcher f) = runStateT (runMaybeT f)

-- | Evaluate a 'FSNodeTest' on the current 'FSearchState' (the value returned by 'ask') in the
-- contest of a 'FileMatcher' function, if the 'FSNodeTest' succeeds, evaluate the given
-- 'FileMatcher' action. The '?->' infix operator is identical to this function.
ftest
  :: [FSNodeTest st] -> FileMatcher st (FileMatchResult a)
  -> FileMatcher st (FileMatchResult a)
ftest preds action = do
  result <- runFSNodeTest (anyFSNodeTest preds) <$> ask
  if result then action else noMatch
infixr 1 `ftest`

(?->)
  :: [FSNodeTest st] -> FileMatcher st (FileMatchResult a)
  -> FileMatcher st (FileMatchResult a)
(?->) = ftest
infixr 1 ?->

----------------------------------------------------------------------------------------------------

-- | Inspect a 'FPath', and if the 'FPath' is a directory, iterate over every 'FSNode's element that
-- exists in the directory as an element of a 'Pipe'. If the given 'FPath' is not a directory, yield
-- a single 'FSNode' in the output 'Pipe'.
--
-- __WARNING:__ The file descriptor to the directory is not closed until the last item in the 'Pipe'
-- is evaluated. This can lead to resource leaks if you do not fully evaluate the directory stream
-- at least once.
--
-- TODO: remove this function from the public API.
listFSNodes :: FPath -> IO (Pipe IO FSNode)
listFSNodes dir = openDirStream (unpack dir) >>= loop where
  loop stream = do
    name <- catch (readDirStream stream) $ \ e ->
      closeDirStream stream >> throwIO (e :: SomeException)
    if null name then closeDirStream stream >> pure PipeStop else do
      node <- fsNode dir $ pack name
      return $ PipeNext node (loop stream)

-- | Like 'find' but allows you to construct a stateful value as the file search proceeds.
search
  :: forall st a
   . FileMatcher st (FileMatchResult a)
  -> st -> FPath -> IO (Pipe IO a)
search pat st = listFSNodes >=> step init where
  init node next = evalEngineT (loop node) EngineState
    { theEngineInputPipe  = next
    , theEngineStateValue = FSearchState
        { theCurrentFSNode      = node
        , theCurrentSearchPath  = [fsNodeName node]
        , theCurrentSearchDepth = 0
        , theCurrentUserState   = st
        }
    }
  next = input >>= \ file -> loop file
  loop node = let name = fsNodeName node in if name == ".." || name == "." then next else do
    engineStateValue . currentFSNode .= node
    (result, st) <- runFileMatcher pat <$> gets theEngineStateValue >>= liftIO
    engineStateValue .= st
    result <- maybe noMatch pure result
    -- The '<|>' operator combines the outputs of each of the search steps, similar to how '<|>'
    -- works for lists data types.
    output (theFileMatchYield result) <|> enterSubdir result node <|> next
  enterSubdir result node = do
    -- First check if this is a directory into which we should recursively search.
    guard $ theFileMatchStepInto result && isDirectory (fsNodeStat node)
    -- Save the old state
    oldstream <- use engineInputPipe
    -- Set the new state using the directory into which we will search recursively.
    engineInputPipe .= listFSNodes (fsNodePath node)
    engineStateValue %=
      ( currentSearchDepth +~ 1 ) .
      ( currentSearchPath  %~ ((fsNodeName node) :) )
    -- Evaluate the next depth recursive step of the search.
    result <- next
    -- Restore the old state.
    engineInputPipe .= oldstream
    engineStateValue %=
      ( currentSearchDepth -~ 1 ) .
      ( currentSearchPath  %~ \ case
          []     -> error "foldDir: stack underflow"
          _:tail -> tail
      )
    -- Push the results of the laste depth recursive search step to the output
    return result

-- "Fast" means subdirectories are not kept open when recursively searching, which can lead to race
-- conditions if other processes are updating the filesystem within the subtree being searched.
fastSearch
  :: MonadIO m
  => EngineT (FSearchState ()) FPath m FSNode
fastSearch = error "TODO: fastSearch"

-- | "List recursive," similar to invoking @ls -r@ on the command line.
lsr :: [FPath] -> IO (Pipe IO FSNode)
lsr = pmap (search ([mempty] ?-> match) ()) . pipe >=> pconcat
