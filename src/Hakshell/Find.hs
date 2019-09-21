-- | In this module a "find"-like command is defined, and there are a few versions with slightly
-- different behavior: 'find' is the most common, which recursively searches a list of directories
-- piped into it, and the content of each directory is dumped into an output pipe. You can then bind
-- the result additional filters that accept or reject each entry.
--
-- Here is a simple example of how you might perform a search for files in two different
-- directories, ".\/developer-docs" and ".\/user-manual" for all files named "index.html" or files
-- named "README.md", with the following 5-line function:
--
-- @
-- 'Hakshell.Pipe.values' [".\/developer-docs", ".\/user-manual"]
--     $ 'search' 'safe'
--         ([ 'file' '<>' 'isNamed' "index.html"
--          , 'file' '<>' 'isNamed' "README.md"] '?->' 'match')
--     $ return
-- @
--
-- The 'values' above is a function in the "Hakshell.Pipe" module which can pipe a list of values to
-- the next function in the chain, which is the 'search' function.
--
-- The 'search' function is given a higher-order function '?->' which takes a pattern
-- @['file' '<>' 'isNamed' "index.html", 'file' '<>' 'isNamed' "README.md"]@
-- and a decision function, 'match'. (Other decision functions are 'noMatch', 'matchPrune', and
-- 'prune'). The 'search' function will recurse through the directories given by
-- 'Hakshell.Pipe.values' and passes all files matching the pattern @['file', 'isNamed'
-- "index.html"]@ and pipes them to the next step in the chain, which is the 'return' function.
--
-- With the 'return' function being the final step in the chain, this entire 5-line function will
-- return all files that match the pattern.
module Hakshell.Find
  ( -- * Executing a Filesystem Query

    search, foldMapFS, FSFoldMap, runFSFoldMap,
    MapDirErr, MapDir,
    fast, safe, fastErr, safeErr,
    MapDirErrHandler, defaultMapDirErrHandler,

    -- * The 'FPath' datatype
    --
    -- A string-like data type for representing filesystem path values.

    FPath(..), pwd, cd,

    -- ** 'FPathList'
    --
    -- A collection of 'FPath' values. These functions read the content of an entire directory into
    -- memory in a single operation.

    FPathList, listFPaths, fPathList, pwdPathList,

    -- * The 'FSNode' datatype
    --
    -- Represents an entry in a filesystem, not just it's path. This includes information about the
    -- file type, it's owners, permission settings, and the size of the file.

    FSNode, fsNode, fsNodePath,

    -- ** 'FSNodeList'
    --
    -- A collection of 'FSNode' values. These functions read the content of an entire directory into
    -- memory in a single operation.

    FSNodeList, fsNodeList, fsNodeListTime, fsNodeListSource,
    pwdListFSNode,

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

    fNamePredicate, fStatusPredicate,

    -- * Querying the Filesystem

    DecideFileMatch(..), prune, noMatch, matchPrune, match,

    -- *** Lenses on 'FileMatchResult's.

    fileMatchStepInto, fileMatchYield, decideFileMatch,

    -- ** Deciding which files match
    --
    -- Functionality for writing predicates on files that can be used during a filesystem search.

    FileMatcher, FTest(..), (?->),
    fileName, searchPath, fullPath, fileStatus, searchDepth,

    -- *** Low-level access to the state of a query
    --
    -- An 'FSearchState' is created by the 'search' function and can be accessed from within a
    -- 'FileMatcher' function via the 'Control.Monad.Reader.ask' function of the
    -- "Control.Monad.Reader" API. Ususally you will not need to use the 'FSearchState' data type
    -- directly, rather you will evaluate some of the pre-defined predicates like 'file' or 'dir'
    -- using the 'ftest' function or the @('?->')@ operator.

    SearchDepth(..),

    -- * Stream editing
    sed,

  ) where

import           Hakshell.Pipe
import           Hakshell.String

import           Control.Arrow
import           Control.Exception
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.State
import           Control.Monad.Trans.Maybe

import           Data.String
import           Data.Time.Clock
import           Data.Void

import           System.Directory
                 ( getCurrentDirectory
                 , getDirectoryContents
                 )
import           System.FilePath( (</>) )
import           System.IO
import           System.Posix.Directory
                 ( openDirStream, readDirStream, closeDirStream, changeWorkingDirectory
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
  deriving (Eq, Ord, Enum, Num, Bounded, Show)

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

cd :: FPath -> IO ()
cd = changeWorkingDirectory . unpack

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
-- In general, the 'Semigroup' every time a concatenation operator '<>' is applied to an
-- 'FSNodeTest', the test should become more restrictive.
--
-- In order to perform a negation (logical NOT), simply use 'fnot'.
--
-- To perform a disjunction (logical OR) of 'FSNodeTest's, use 'eitherFSNodeTest' or
-- 'anyFSNodeTest', however the 'FileMatcher' functions 'ftest' and the operator @'?->'@ takes a
-- list of 'FSNodeTest' functions and performs a logical disjunction anyway, so usually the
-- 'eitherFSNodeTest' function is not necessary, just construct a list of 'FSNodeTest's.
data FSNodeTest st
  = FSNodeTest
    { theFSNodeTestMinDepth   :: Maybe (Max SearchDepth)
      -- minDepth is wrapped in 'Max' so it increases with each concatenation
    , theFSNodeTestMaxDepth   :: Maybe (Min SearchDepth)
      -- maxDepth is wrapped in 'Min' so it decreases with each concatenation
    , theFSNodeTestSearchPath :: Maybe ([FPath] -> All)
    , theFSNodeTestFileName   :: Maybe (FPath -> All)
    , theFSNodeTestStatus     :: Maybe (FileStatus -> All)
    , theFSNodeTestState      :: Maybe (st -> All)
    }

instance Semigroup (FSNodeTest st) where
  (FSNodeTest a1 a2 a3 a4 a5 a6) <> (FSNodeTest b1 b2 b3 b4 b5 b6) =
    FSNodeTest (a1<>b1) (a2<>b2) (a3<>b3) (a4<>b4) (a5<>b5) (a6<>b6)  

instance Monoid (FSNodeTest st) where { mempty = emptyFSNodeTest; mappend = (<>); }

emptyFSNodeTest :: FSNodeTest st
emptyFSNodeTest = let n = Nothing in FSNodeTest n n n n n n

fsNodeTestSearchPath :: Lens' (FSNodeTest st) (Maybe ([FPath] -> All))
fsNodeTestSearchPath = lens theFSNodeTestSearchPath $ \ a b -> a{ theFSNodeTestSearchPath = b }

fsNodeTestFileName :: Lens' (FSNodeTest st) (Maybe (FPath -> All))
fsNodeTestFileName = lens theFSNodeTestFileName $ \ a b -> a{ theFSNodeTestFileName = b }

fsNodeTestStatus   :: Lens' (FSNodeTest st) (Maybe (FileStatus -> All))
fsNodeTestStatus = lens theFSNodeTestStatus $ \ a b -> a{ theFSNodeTestStatus = b }

fsNodeTestState    :: Lens' (FSNodeTest st) (Maybe (st -> All))
fsNodeTestState = lens theFSNodeTestState $ \ a b -> a{ theFSNodeTestState = b }

-- evaluate 'not' on the result of an 'All' predicate within a 'Maybe' constructor.
mAllNot :: Maybe (n -> All) -> Maybe (n -> All)
mAllNot = fmap (fmap (All . not . getAll))

-- | Compute the logical inversion the 'FSNodeTest', i.e. if the test fails then the 'fnot' of the
-- test passes. This inversion is not applied to 'fsNodeTestMinDepth' or 'fsNodeTestMaxDepth'.
fnot :: FSNodeTest st -> FSNodeTest st
fnot = (fsNodeTestSearchPath %~ mAllNot) . (fsNodeTestFileName %~ mAllNot) .
       (fsNodeTestStatus     %~ mAllNot) . (fsNodeTestState    %~ mAllNot)

-- logical disjunction of two conjunctive predicates wrapped in 'Maybe' data types.
mAllOR :: Maybe (n -> All) -> Maybe (n -> All) -> Maybe (n -> All)
mAllOR b a = (\ a b n -> All $ getAll (a n) || getAll (b n)) <$> a <*> b <|> a <|> b

-- Use a lens to apply 'mAllOR' to a logical field within two 'FSNodeTests'.
mAllORlens
  :: Lens' (FSNodeTest st) (Maybe (n -> All))
  -> FSNodeTest st -> State (FSNodeTest st) ()
mAllORlens lens b = lens %= mAllOR (b ^. lens)

-- | Perform a disjunction (logical OR) of 'FSNodeTest's, use 'eitherFSNodeTest' or
-- 'anyFSNodeTest'. To perform a negation (logical NOT) use 'fnot'
eitherFSNodeTest :: FSNodeTest st -> FSNodeTest st -> FSNodeTest st
eitherFSNodeTest = flip $ \ b -> execState $ do
  mAllORlens fsNodeTestSearchPath b
  mAllORlens fsNodeTestFileName   b
  mAllORlens fsNodeTestStatus     b
  mAllORlens fsNodeTestState      b

-- | Fold 'eitherFSNodeTest' on a list of 'FSNodeTest's, this is the 'FSNodeTest' analogue of the
-- 'Prelude.or' function.
anyFSNodeTest :: [FSNodeTest st] -> FSNodeTest st
anyFSNodeTest = foldl eitherFSNodeTest mempty

-- | This function converts a predicate on a 'System.Posix.Files.FileStatus' value and converts it
-- to a 'FSNodeTest' predicate. This means you pass a predicate function from the
-- "System.Posix.Files" module, and produce a 'FSNodeTest' function that evaluates the predicate on
-- the 'fsNodeStat' property of an 'FSNode' data type.
fStatusPredicate :: (FileStatus -> Bool) -> FSNodeTest st
fStatusPredicate f = emptyFSNodeTest{ theFSNodeTestStatus = Just $ All <$> f }

-- | Matches directories
dir :: FSNodeTest st
dir = fStatusPredicate isDirectory

-- | Matches regular files
file :: FSNodeTest st
file = fStatusPredicate isRegularFile

-- | Matches UNIX sockets
socket :: FSNodeTest st
socket = fStatusPredicate isSocket

-- | Matches UNIX named pipes, also sometimes called "FIFOs".
fifo :: FSNodeTest st
fifo = fStatusPredicate isNamedPipe

-- | Matches symbolic links
link :: FSNodeTest st
link = fStatusPredicate isSymbolicLink

-- | Evaluates a predicate on the 'fsNodeName' of an 'FSNode'.
fNamePredicate :: (FPath -> Bool) -> FSNodeTest st
fNamePredicate f = emptyFSNodeTest{ theFSNodeTestFileName = Just $ All <$> f }

-- | File name (the basename) matches the exact string given:
isNamed :: FPath -> FSNodeTest st
isNamed = fNamePredicate . (==)

----------------------------------------------------------------------------------------------------

-- | A 'FileMatcher' must return a 'FileMatch' value, which is used by the 'search' function to
-- control the behavior of the query after a file matches or does not match.
--
-- The 'ftest' function
data DecideFileMatch a
  = DecideFileMatch
    { theFileMatchStepInto :: !Bool
      -- ^ If this file match is scrutinizing a directory, the 'search' function must decide whether
      -- to step into this directory (depth first), set this value to tell 'search' whether or not
      -- to do so. Setting this value to 'False' will prune the directory under scrutiny from
      -- recursively being 'search'ed.
    , theFileMatchMaxDepth :: Maybe SearchDepth
    , theFileMatchYield    :: Pipe a
      -- ^ A file match result may yield zero or more values.
    }
  deriving Functor

-- | This function is used within a 'FileMatcher' function. Do not output a value, do not step into
-- this subdirectory if the current match is scrutinizing a subdirectory,
prune :: Applicative m => m (DecideFileMatch a)
prune = pure $ DecideFileMatch False Nothing empty

-- | This function is used within a 'FileMatcher' function. Do not output any value, but do step
-- into this subdirectory if the current match is scrutinizing a subdirectory. This is the default
-- behavior for the 'ftest' function.
noMatch :: Applicative m => m (DecideFileMatch a)
noMatch = pure $ DecideFileMatch True Nothing empty

-- | Output the file being scrutinized, but do not step into this subdirectory if the current match
-- is scrutinizing a subdirectory.
matchPrune :: FileMatcher st (DecideFileMatch FPath)
matchPrune = DecideFileMatch False Nothing . pure <$> fullPath

-- | Output the file being scrutinized, and do step into this subdirectory if the current match is
-- scrutinizing a subdirectory.
match :: FileMatcher st (DecideFileMatch FSNode)
match = DecideFileMatch True Nothing . pure <$> fileNode

-- | If this file match is scrutinizing a directory, the 'search' function must decide whether to
-- step into this directory (depth first), set this value to tell 'search' whether or not to do
-- so. Setting this value to 'False' will prune the directory under scrutiny from recursively being
-- 'search'ed.
fileMatchStepInto :: Lens' (DecideFileMatch a) Bool
fileMatchStepInto = lens theFileMatchStepInto $ \ a b -> a{ theFileMatchStepInto = b }

fileMatchYield :: Lens' (DecideFileMatch a) (Pipe a)
fileMatchYield = lens theFileMatchYield $ \ a b -> a{ theFileMatchYield = b }

fileMatchMaxDepth :: Lens' (DecideFileMatch a) (Maybe SearchDepth)
fileMatchMaxDepth = lens theFileMatchMaxDepth $ \ a b -> a{ theFileMatchMaxDepth = b }

-- | Construct a default 'FileMatch' value, the default behavior is to set the 'fileMatchStepInto'
-- value to 'True'.
decideFileMatch :: Pipe a -> DecideFileMatch a
decideFileMatch = DecideFileMatch True Nothing

----------------------------------------------------------------------------------------------------

-- | Functions of this type scrutinize a file node in the file system. Evaluate functions of this
-- type using the 'ftest' function, or equivalently, the @('?->')@ operator. The 'ftest' expression
-- can then be passed as an argument to the 'search' or 'find' functions. For example:
--
-- @
-- 'find' [".\/subdirA", ".\/subdirB"] $
--     [ ['file' '<>' 'isNamed' "index.html", 'file' '<>' 'isNamed' "robots.txt"] '?->' 'match'
--     , ['dir' '<>' 'isNamed' "pics"] '?->' 'prune'
--     ]
-- @
newtype FileMatcher st a
  = FileMatcher (MaybeT (StateT (FSearchState st) IO) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO)

instance MonadState st (FileMatcher st) where
  state f = FileMatcher $ lift $ state $ \ st0 ->
    let (a, st) = f (st0 ^. searchUserStateLens) in (a, st0 & searchUserStateLens .~ st)

-- | This function contains the current state of a 'search' operation.
data FSearchState st
  = FSearchState
    { theSearchFileName   :: FPath
    , theSearchDirectory  :: [FPath]
    , theSearchFileStatus :: Maybe FileStatus
    , theSearchFullPath   :: Maybe FPath
    , theSearchDepth      :: !SearchDepth
    , theSearchUserState  :: st
    }

initFSearchState :: FPath -> st -> FSearchState st
initFSearchState path st = FSearchState
  { theSearchFileName   = path
  , theSearchDirectory  = []
  , theSearchFileStatus = Nothing
  , theSearchFullPath   = Nothing
  , theSearchDepth      = 0
  , theSearchUserState  = st
  }

askSearchEnv :: FileMatcher st (FSearchState st)
askSearchEnv = FileMatcher $ lift get

asksSearchEnv :: (FSearchState st -> a) -> FileMatcher st a
asksSearchEnv = (<$> askSearchEnv)

viewSearchEnv :: Lens' (FSearchState st) a -> FileMatcher st a
viewSearchEnv = asksSearchEnv . view

searchFileNameLens :: Lens' (FSearchState st) FPath
searchFileNameLens = lens theSearchFileName $ \ a b -> a{ theSearchFileName = b }

searchFileStatusLens :: Lens' (FSearchState st) (Maybe FileStatus)
searchFileStatusLens = lens theSearchFileStatus $ \ a b -> a{ theSearchFileStatus = b }

searchFullPathLens :: Lens' (FSearchState st) (Maybe FPath)
searchFullPathLens = lens theSearchFullPath $ \ a b -> a{ theSearchFullPath = b }

searchDirectoryLens :: Lens' (FSearchState st) [FPath]
searchDirectoryLens = lens theSearchDirectory $ \ a b -> a{ theSearchDirectory = b }

searchDepthLens :: Lens' (FSearchState st) SearchDepth
searchDepthLens = lens theSearchDepth $ \ a b -> a{ theSearchDepth = b }

searchUserStateLens :: Lens' (FSearchState st) st
searchUserStateLens = lens theSearchUserState $ \ a b -> a{ theSearchUserState = b }

-- | Get the name of the current file being scrutinized.
fileName :: FileMatcher st FPath
fileName = viewSearchEnv searchFileNameLens

-- | Get the name of the path that has been walked up to the current file being scrutinized. The
-- 'fileName' is not included, and the path is in 'reverse' order, meaning the first (top-most)
-- subdirectory scanned is the final item in the list while the 'head' of the list is the latest
-- (bottom-most) subdirectory to have been scanned.q
searchPath :: FileMatcher st [FPath]
searchPath = viewSearchEnv searchDirectoryLens

-- not for export
maybeGetFullPath :: FSearchState st -> FPath
maybeGetFullPath st = case st ^. searchFullPathLens of
  Just path -> path
  Nothing   -> pack $ foldl (flip (</>))
    (unpack $ st ^. searchFileNameLens)
    (unpack <$> (st ^. searchDirectoryLens))

-- | Join 'fileName' and 'searchPath' together into a single long 'FPath' value using the @('</>')@
-- operator.
fullPath :: FileMatcher st FPath
fullPath = viewSearchEnv searchFullPathLens >>= flip maybe return
  (do path <- asksSearchEnv maybeGetFullPath
      FileMatcher $ searchFullPathLens .= Just path
      return path
  )

-- not for export
maybeGetFileStatus :: MonadIO m => FSearchState st -> m FileStatus
maybeGetFileStatus st = case st ^. searchFileStatusLens of
  Nothing   -> liftIO $ getFileStatus $ unpack $ maybeGetFullPath st
  Just stat -> return stat

-- | Obtain the 'FileStatus' of the current file being scrutinized. If the 'FileStatus' has not
-- already been obtained from the filesystem, the 'getFileStatus' function is called.
fileStatus :: FileMatcher st FileStatus
fileStatus = viewSearchEnv searchFileStatusLens >>= flip maybe return
  (do stat <- askSearchEnv >>= maybeGetFileStatus
      FileMatcher $ searchFileStatusLens .= Just stat
      return stat
  )

-- | Obtain an 'FSNode' for the current file being scrutinized.
fileNode :: FileMatcher st FSNode
fileNode = FSNode
  <$> (pack . foldl (flip (</>)) "" . fmap unpack <$> searchPath)
  <*> fileName
  <*> fileStatus

-- | Return a number indicating how many levels deep into the filesystem tree that this search has
-- traversed.
searchDepth :: FileMatcher st SearchDepth
searchDepth = viewSearchEnv searchDepthLens

-- | Return the user-defined state value
searchState :: FileMatcher st st
searchState = viewSearchEnv searchUserStateLens

-- not for export
--
-- Evaluation requires an 'FSearchState' value, and it would be best if this data type is not
-- exported or manipulated by the users in any way, the content needs to be set mechanically for it
-- to be useful.
runFileMatcher :: FileMatcher st a -> FSearchState st -> IO (Maybe a, FSearchState st)
runFileMatcher (FileMatcher f) = runStateT (runMaybeT f)

ifM :: Monad m => m a -> m a -> Bool -> m a
ifM yes no pass = if pass then yes else no

evalFSNodeTest :: forall st . FSNodeTest st -> FileMatcher st Bool
evalFSNodeTest test =
  eval (fmap (fmap All . (<=) . getMax) . theFSNodeTestMinDepth) searchDepth $
  eval theFSNodeTestState      searchState $
  eval theFSNodeTestFileName   fileName    $
  eval theFSNodeTestSearchPath searchPath  $
  eval theFSNodeTestStatus     fileStatus  $
  return True where
    eval :: (FSNodeTest st -> Maybe (a -> All))
         -> FileMatcher m a
         -> FileMatcher m Bool
         -> FileMatcher m Bool
    eval take f next =
      maybe (return True) ((<$> f) . fmap getAll) (take test) >>= ifM next (return False)

----------------------------------------------------------------------------------------------------

-- | This function type is used to evaluate a mapping over the files in a file system, performing a
-- fold as it goes. It lifts the 'ContT' monad transformer so that you can make use of 'callCC', the
-- concrete type value that will be bound to continuation return type variable @cr@ is bound by the
-- 'runFSFoldMap' function evaluation.
newtype FSFoldMap cr st a = FSFoldMap{ unwrapFSFoldMap :: ContT cr (StateT st IO) (Pipe a) }
  deriving Functor

newtype FSFoldMapHalt cr st a = FSFoldMapHalt (a -> FSFoldMap cr st Void)

instance Applicative (FSFoldMap cr st) where
  pure = FSFoldMap . pure . pure
  (FSFoldMap f) <*> (FSFoldMap a) = FSFoldMap $ (<*>) <$> f <*> a

instance Alternative (FSFoldMap cr st) where
  empty = FSFoldMap $ return empty
  (FSFoldMap a) <|> (FSFoldMap b) = FSFoldMap $ (<|>) <$> a <*> b

instance Monad (FSFoldMap cr st) where
  return = FSFoldMap . return . pure
  (FSFoldMap a) >>= f = FSFoldMap $ fmap (unwrapFSFoldMap . f) <$> a >>= fmap join . sequence

instance MonadPlus (FSFoldMap cr st) where { mzero = empty; mplus = (<|>); }

instance MonadState st (FSFoldMap cr st) where
  state = FSFoldMap . fmap pure . state

instance MonadCont (FSFoldMap cr st) where
  callCC a = FSFoldMap $ callCC $ \ b -> unwrapFSFoldMap $ a (FSFoldMap . b . pure)

instance MonadIO (FSFoldMap cr st) where
  liftIO = FSFoldMap . liftIO . fmap pure

instance MonadPipe (FSFoldMap cr st) where
  yield = FSFoldMap . pure

----------------------------------------------------------------------------------------------------

-- | Construct a predicate and associate it with a decision as to whether or not to yield a value
-- and also, if the file node under scrutiny is a directory, whether or not to recurse into the
-- subdirectory
data FTest st a
  = FTest
    { fTestPredicate :: [FSNodeTest st]
    , fTestDecision  :: FileMatcher st (DecideFileMatch a)
    }
  deriving Functor
infixl 4 `FTest`

(?->) :: [FSNodeTest st] -> FileMatcher st (DecideFileMatch a) -> FTest st a
(?->) = FTest
infixl 4 ?->

-- not for export -- requires initializing 'FSearchState', which must be done mechanically to avoid
-- mistakes.
--
-- This function evaluate a 'FSNodeTest' on the current 'FSearchState' in the contest of a
-- 'FileMatcher' function, if the 'FSNodeTest' succeeds, evaluate the given 'FileMatcher' action.
runFTest
  :: FTest st file -> FSearchState st
  -> FSFoldMap cr st (Maybe (DecideFileMatch file), FSearchState st)
runFTest test st = liftIO $ flip runFileMatcher st $ foldr
  (\ p next ->
    ( evalFSNodeTest p >>= flip ifM empty
      ((fileMatchMaxDepth .~ (getMin <$> theFSNodeTestMaxDepth p)) <$> fTestDecision test)
    ) <|> next
  ) empty (fTestPredicate test)

-- | Evaluate an 'FSFoldMap' function, returning the final return value paired with the final state
-- value.
runFSFoldMap :: FSFoldMap cr st a -> (Pipe a -> StateT st IO cr) -> st -> IO (cr, st)
runFSFoldMap (FSFoldMap f) = runStateT . runContT f

-- | This is an interface method used internally by the 'foldMapFS' function. Functions of this type
-- provide a list of files over which the fold-map operation should evaluate. There are two
-- different methods of providing files: keeping the directory open as the fold-map evaluates (this
-- is done by 'safeErr'), and buffering the contents of the directory so it is not kept open as
-- the fold-map evaluates (this is done by 'fastErr').
type MapDirErr cr st a = MapDirErrHandler cr st a -> MapDir cr st a

-- | Takes a function which is mapped to every directory, which may also (in a way) perform a fold
-- on any value of a type that you provide for @st@ by way of the "Control.Monad.State" functions
-- 'get', 'put', 'modify', and 'state'.
type MapDir cr st a = (FPath -> FPath -> FSFoldMap cr st a) -> FPath -> FSFoldMap cr st a

-- | This function responds to 'IOException's thrown when the operating system indicates a file or
-- directory cannot be inspected by the 'search' or 'foldMapFS' functions.
type MapDirErrHandler cr st a = FPath -> IOException -> FSFoldMap cr st a

-- | This is a default 'MapDirErrHandler' which you can pass as an argument to a pre-defined
-- 'MapDirErr' function in order to construct a 'MapDir' function that can be passed as an argument
-- to 'search' or 'foldMapFS'. This is the error handler used by 'safe' and 'mapListDir'.
defaultMapDirErrHandler :: MapDirErrHandler cr st a
defaultMapDirErrHandler parent err =
  liftIO (hPutStrLn stderr $ show parent ++ ": " ++ show err) >> empty

-- | By applying a function of type 'MapDirErrHandler' to this function, you can construct a
-- 'MapDir' function that can be used to evaluate the 'search' and 'foldMapFS' functions.
--
-- This function opens a directory, obtains the name of each file in that directory, and maps the
-- given 'FSFoldMap' function to each file name. Then, after all file names have been mapped, this
-- function closes the directory.
--
-- The first function provided must be an error handling function in the event that a file could not
-- be read -- feel free to re-throw the exception, the directory file descriptor will be closed
-- properly if you do.
safeErr :: MapDirErr cr st a
safeErr catcher f dir = do
  stream <- liftIO $ openDirStream $ unpack dir
  let loop = liftIO (try $ readDirStream stream) >>= \ case
        Right file ->
          if null file
           then liftIO (try $ closeDirStream stream) >>= \ case
            Right () -> empty
            Left err -> catcher dir err
           else f dir (pack file) <|> loop
        Left   err -> catcher dir err <|> loop
  FSFoldMap $ ContT $ \ next -> StateT $ \ st -> runFSFoldMap loop next st

-- | Same as 'safeErr' except 'IOExceptions' that occur while reading the directory are printed to
-- 'stderr' and then ignored. This function is simply 'safeErr' with 'defaultMapDirErrHandler'
-- applied as the first argument to that function.
safe :: MapDir cr st a
safe = safeErr defaultMapDirErrHandler

-- | By applying a function of type 'MapDirErrHandler' to this function, you can construct a
-- 'MapDir' function that can be used to evaluate the 'search' and 'foldMapFS' functions.
--
-- This function opens a directory, copies the names of all files in that directory into a buffer,
-- and closes the directory. Then, after closing the directory, this function maps the given
-- 'FSFoldMap' function to each file name in the buffer.
--
-- The first function provided must be an error handling function in the event that a file could not
-- be read -- feel free to re-throw the exception, the directory file descriptor will be closed
-- properly if you do.
fastErr :: MapDirErr cr st a
fastErr catcher f dir = do
  stream <- liftIO $ openDirStream $ unpack dir
  let buffer results stack = liftIO (try $ readDirStream stream) >>= \ case
        Left   err -> catcher dir err >>= \ a -> buffer (results <|> pure a) stack
        Right file ->
          if null file then return (results, stack) else buffer results (pack file : stack)
      -- TODO: 1. consider using a Vector buffer instead of a list
      -- TODO: 2. ordering of output directories is unexpected compared to that of 'safeErr'
  let loop = \ case
        []         -> liftIO (closeDirStream stream) >> empty
        file:stack -> f dir file <|> loop stack
  (results, stack) <- FSFoldMap $ ContT $ \ next -> StateT $ \ st ->
    runFSFoldMap (buffer empty []) next st
  loop stack <|> yield results

-- | Same as 'safeErr' except 'IOExceptions' that occur while reading the directory are printed to
-- 'stderr' and then ignored. This function is simply 'safeErr' with 'defaultMapDirErrHandler'
-- applied as the first argument to that function.
fast :: MapDir cr st a
fast = fastErr defaultMapDirErrHandler

-- not for export -- too complicated to be useful
searchLoop
  :: MapDir cr st a
  -> FTest st file -> FSFoldMapHalt cr st a -> FSearchState st
  -> (file -> FSFoldMap cr st a)
  -> FSFoldMap cr st a
searchLoop mapDir test halt st cont = do
  (decision, st) <- runFTest test st
  (maybe empty (yield . theFileMatchYield >=> cont) decision) <|> do
    guard $ uncurry (&&) $ ((/= ".") &&& (/= "..")) (theSearchFileName st)
    guard $ flip (maybe True) decision $ \ decision ->
      theFileMatchStepInto decision &&
      maybe True ((theSearchDepth st) <=) (decision ^. fileMatchMaxDepth)
    maybeGetFileStatus st >>= guard . isDirectory
    flip mapDir (maybeGetFullPath st) $ \ _parent path -> flip (searchLoop mapDir test halt) cont $
      st{ theSearchFileName   = path
        , theSearchDirectory  = theSearchFileName st : theSearchDirectory st
        , theSearchFileStatus = Nothing
        , theSearchFullPath   = Nothing
        , theSearchDepth      = 1 + theSearchDepth st
        }

-- | This function works somewhat similar to how the @find@ program works in a command line
-- environment on a typical UNIX or Linux system. The 'FTest' is used to determine which files are
-- selected, and on each selected file a function of type 'FSFoldMap' can be evaluated. A slightly
-- more complex version of this function, 'foldMapFS' allows you to pass a stateful value that can
-- be updated on each found @file@.
--
-- The first argument to this function must be a function of type 'MapDir', two of which are most
-- convenient: 'fast' and 'safe'. Thus, invoke this function as @'search' 'fast'@ or @'search'
-- 'safe'@, then apply an 'FTest' pattern as the following argument. The 'search' 'fast' function
-- emulates the default behavior of the @find@ command line program, whereas the 'search' 'safe'
-- function emulates the behavior of the @find@ command when the @-execdir@ parameter is given.
search
  :: PipeLike pipe
  => MapDir (Pipe a) () a
  -> FTest () file
  -> (file -> FSFoldMap (Pipe a) () a)
  -> pipe FPath -> IO (Pipe a)
search mapDir test = foldMapFS mapDir test () (const . pure)

-- | Like 'search', but provide a state value to fold values into as the 'search' operation
-- proceeds. Also it is necessary to provide a final evaluator
foldMapFS
  :: PipeLike pipe
  => MapDir (Pipe a) st a
  -> FTest st file -- ^ the file selection rules
  -> st -- ^ the initial state
  -> (Pipe a -> st -> IO b) -- ^ the final action to evaluate, after 'search' completes
  -> (file -> FSFoldMap (Pipe a) st a) -- ^ the action to evaluate on each file found.
  -> pipe FPath -> IO b
foldMapFS mapDir test st final cont paths = runFSFoldMap
  ( callCC $ \ halt ->
      ( forM (pipe paths) $ \ path ->
          get >>= flip (searchLoop mapDir test (FSFoldMapHalt halt)) cont . initFSearchState path
      ) >>= yield
  ) return st >>= uncurry final

----------------------------------------------------------------------------------------------------

type SedHalt void m a = Pipe a -> ContT (Pipe a) m void

-- | Stream edit: this function opens a file path and breaks the content of it into lines of
-- text. You can then apply a function to the lines of text, and either return the line in it's
-- present form or not.
sed :: forall void m a . MonadIO m
    => (SedHalt void m a -> String -> ContT (Pipe a) m (Pipe a)) -> FPath -> m (Pipe a)
sed f path =
  liftIO (openFile (unpack path) ReadMode >>= hGetContents) >>= \ content ->
  runContT (callCC $ \ halt -> loop empty (lines content) halt) return
  where
    loop :: MonadIO m => Pipe a -> [String] -> SedHalt void m a -> ContT (Pipe a) m (Pipe a)
    loop pip0 lns halt = case lns of
      []      -> return pip0
      ln:more -> f (halt . mappend pip0) ln >>= \ pip1 -> loop (pip0 <|> pip1) more halt
