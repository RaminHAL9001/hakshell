-- | In this module a "find"-like command is defined, and there are a few versions with slightly
-- different behavior: 'find' is the most common, which recursively searches a list of directories
-- piped into it, and the content of each directory is dumped into an output pipe. You can then bind
-- the result additional filters that accept or reject each entry.
module Hakshell.Find
  ( -- * Executing a Filesystem Query

    search, FSFoldMap, runFSFoldMap, mapDir, mapDirErr,

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

    FileMatchResult(..), prune, noMatch, matchPrune, match,

    -- *** Lenses on 'FileMatchResult's.

    fileMatchStepInto, fileMatchYield, fileMatchResult,

    -- ** Deciding which files match
    --
    -- Functionality for writing predicates on files that can be used during a filesystem search.

    FileMatcher, FTest(..), (?->),
    fileName, searchPath, fullPath, fileStatus, searchDepth, runFTest,

    -- *** Low-level access to the state of a query
    --
    -- An 'FSearchState' is created by the 'search' function and can be accessed from within a
    -- 'FileMatcher' function via the 'Control.Monad.Reader.ask' function of the
    -- "Control.Monad.Reader" API. Ususally you will not need to use the 'FSearchState' data type
    -- directly, rather you will evaluate some of the pre-defined predicates like 'file' or 'dir'
    -- using the 'ftest' function or the @('?->')@ operator.

    FSearchState, theCurrentFileName,
    theCurrentSearchDepth, SearchDepth(..),
    theCurrentSearchPath,

  ) where

import           Hakshell.Pipe
import           Hakshell.String

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

fsNodeTestMinDepth :: Lens' (FSNodeTest st) (Maybe (Max SearchDepth))
fsNodeTestMinDepth = lens theFSNodeTestMinDepth $ \ a b -> a{ theFSNodeTestMinDepth = b }

fsNodeTestMaxDepth :: Lens' (FSNodeTest st) (Maybe (Min SearchDepth))
fsNodeTestMaxDepth = lens theFSNodeTestMaxDepth $ \ a b -> a{ theFSNodeTestMaxDepth = b }

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
data FileMatchResult a
  = FileMatchResult
    { theFileMatchStepInto :: !Bool
      -- ^ If this file match is scrutinizing a directory, the 'search' function must decide whether
      -- to step into this directory (depth first), set this value to tell 'search' whether or not
      -- to do so. Setting this value to 'False' will prune the directory under scrutiny from
      -- recursively being 'search'ed.
    , theFileMatchYield    :: Pipe a
      -- ^ A file match result may yield zero or more values.
    }
  deriving Functor

-- | This function is used within a 'FileMatcher' function. Do not output a value, do not step into
-- this subdirectory if the current match is scrutinizing a subdirectory,
prune :: Applicative m => m (FileMatchResult a)
prune = pure $ FileMatchResult False empty

-- | This function is used within a 'FileMatcher' function. Do not output any value, but do step
-- into this subdirectory if the current match is scrutinizing a subdirectory. This is the default
-- behavior for the 'ftest' function.
noMatch :: Applicative m => m (FileMatchResult a)
noMatch = pure $ FileMatchResult True empty

-- | Output the file being scrutinized, but do not step into this subdirectory if the current match
-- is scrutinizing a subdirectory.
matchPrune :: FileMatcher st (FileMatchResult FPath)
matchPrune = FileMatchResult False . pure <$> fullPath

-- | Output the file being scrutinized, and do step into this subdirectory if the current match is
-- scrutinizing a subdirectory.
match :: FileMatcher st (FileMatchResult FSNode)
match = FileMatchResult True . pure <$> fileNode

-- | If this file match is scrutinizing a directory, the 'search' function must decide whether to
-- step into this directory (depth first), set this value to tell 'search' whether or not to do
-- so. Setting this value to 'False' will prune the directory under scrutiny from recursively being
-- 'search'ed.
fileMatchStepInto :: Lens' (FileMatchResult a) Bool
fileMatchStepInto = lens theFileMatchStepInto $ \ a b -> a{ theFileMatchStepInto = b }

fileMatchYield :: Lens' (FileMatchResult a) (Pipe a)
fileMatchYield = lens theFileMatchYield $ \ a b -> a{ theFileMatchYield = b }

-- | Construct a default 'FileMatch' value, the default behavior is to set the 'fileMatchStepInto'
-- value to 'True'.
fileMatchResult :: Pipe a -> FileMatchResult a
fileMatchResult = FileMatchResult True

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
    let (a, st) = f (st0 ^. currentUserStateLens) in (a, st0 & currentUserStateLens .~ st)

-- | This function contains the current state of a 'search' operation.
data FSearchState st
  = FSearchState
    { theCurrentFileName    :: FPath
    , theCurrentSearchPath  :: [FPath]
    , theCurrentFileStatus  :: Maybe FileStatus
    , theCurrentFullPath    :: Maybe FPath
    , theCurrentSearchDepth :: !SearchDepth
    , theCurrentUserState   :: st
    }

initFSearchState :: FPath -> st -> FSearchState st
initFSearchState path st = FSearchState
  { theCurrentFileName    = path
  , theCurrentSearchPath  = []
  , theCurrentFileStatus  = Nothing
  , theCurrentFullPath    = Nothing
  , theCurrentSearchDepth = 0
  , theCurrentUserState   = st
  }

askSearchEnv :: FileMatcher st (FSearchState st)
askSearchEnv = FileMatcher $ lift get

asksSearchEnv :: (FSearchState st -> a) -> FileMatcher st a
asksSearchEnv = (<$> askSearchEnv)

viewSearchEnv :: Lens' (FSearchState st) a -> FileMatcher st a
viewSearchEnv = asksSearchEnv . view

currentFileNameLens :: Lens' (FSearchState st) FPath
currentFileNameLens = lens theCurrentFileName $ \ a b -> a{ theCurrentFileName = b }

currentFileStatusLens :: Lens' (FSearchState st) (Maybe FileStatus)
currentFileStatusLens = lens theCurrentFileStatus $ \ a b -> a{ theCurrentFileStatus = b }

currentFullPathLens :: Lens' (FSearchState st) (Maybe FPath)
currentFullPathLens = lens theCurrentFullPath $ \ a b -> a{ theCurrentFullPath = b }

currentSearchPathLens :: Lens' (FSearchState st) [FPath]
currentSearchPathLens = lens theCurrentSearchPath $ \ a b -> a{ theCurrentSearchPath = b }

currentSearchDepthLens :: Lens' (FSearchState st) SearchDepth
currentSearchDepthLens = lens theCurrentSearchDepth $ \ a b -> a{ theCurrentSearchDepth = b }

currentUserStateLens :: Lens' (FSearchState st) st
currentUserStateLens = lens theCurrentUserState $ \ a b -> a{ theCurrentUserState = b }

-- | Get the name of the current file being scrutinized.
fileName :: FileMatcher st FPath
fileName = viewSearchEnv currentFileNameLens

-- | Get the name of the path that has been walked up to the current file being scrutinized. The
-- 'fileName' is not included, and the path is in 'reverse' order, meaning the first (top-most)
-- subdirectory scanned is the final item in the list while the 'head' of the list is the latest
-- (bottom-most) subdirectory to have been scanned.q
searchPath :: FileMatcher st [FPath]
searchPath = viewSearchEnv currentSearchPathLens

-- not for export
maybeGetFullPath :: FSearchState st -> FPath
maybeGetFullPath st = case st ^. currentFullPathLens of
  Just path -> path
  Nothing   -> pack $ foldr (</>)
    (unpack $ st ^. currentFileNameLens)
    (unpack <$> (st ^. currentSearchPathLens))

-- | Join 'fileName' and 'searchPath' together into a single long 'FPath' value using the @('</>')@
-- operator.
fullPath :: FileMatcher st FPath
fullPath = viewSearchEnv currentFullPathLens >>= flip maybe return
  (do path <- asksSearchEnv maybeGetFullPath
      FileMatcher $ currentFullPathLens .= Just path
      return path
  )

-- not for export
maybeGetFileStatus :: MonadIO m => FSearchState st -> m FileStatus
maybeGetFileStatus st = case st ^. currentFileStatusLens of
  Nothing   -> liftIO $ getFileStatus $ unpack $ maybeGetFullPath st
  Just stat -> return stat

-- | Obtain the 'FileStatus' of the current file being scrutinized. If the 'FileStatus' has not
-- already been obtained from the filesystem, the 'getFileStatus' function is called.
fileStatus :: FileMatcher st FileStatus
fileStatus = viewSearchEnv currentFileStatusLens >>= flip maybe return
  (do stat <- askSearchEnv >>= maybeGetFileStatus
      FileMatcher $ currentFileStatusLens .= Just stat
      return stat
  )

-- | Obtain an 'FSNode' for the current file being scrutinized.
fileNode :: FileMatcher st FSNode
fileNode = FSNode
  <$> (pack . foldr (</>) "" . fmap unpack <$> searchPath)
  <*> fileName
  <*> fileStatus

-- | Return a number indicating how many levels deep into the filesystem tree that this search has
-- traversed.
searchDepth :: FileMatcher st SearchDepth
searchDepth = viewSearchEnv currentSearchDepthLens

-- | Return the user-defined state value
searchState :: FileMatcher st st
searchState = viewSearchEnv currentUserStateLens

-- not for export
--
-- Evaluation requires an 'FSearchState' value, and it would be best if this data type is not
-- exported or manipulated by the users in any way, the content needs to be set mechanically for it
-- to be useful.
runFileMatcher :: FileMatcher st a -> FSearchState st -> IO (Maybe a, FSearchState st)
runFileMatcher (FileMatcher f) = runStateT (runMaybeT f)

iff :: Monad m => m a -> m a -> Bool -> m a
iff yes no pass = if pass then yes else no

evalFSNodeTest :: forall st . FSNodeTest st -> FileMatcher st Bool
evalFSNodeTest test =
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
      maybe (return True) ((<$> f) . fmap getAll) (take test) >>= iff next (return False)

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
    , fTestDecision  :: FileMatcher st (FileMatchResult a)
    }
  deriving Functor
infixl 4 `FTest`

(?->) :: [FSNodeTest st] -> FileMatcher st (FileMatchResult a) -> FTest st a
(?->) = FTest
infixl 4 ?->

-- | Evaluate a 'FSNodeTest' on the current 'FSearchState' (the value returned by 'ask') in the
-- contest of a 'FileMatcher' function, if the 'FSNodeTest' succeeds, evaluate the given
-- 'FileMatcher' action. 
runFTest
  :: FTest st a -> FSearchState st
  -> (FSearchState st -> FileMatchResult a -> FSFoldMap cr st a)
  -> FSFoldMap cr st a
runFTest test st action = case fTestPredicate test of
  []   -> empty
  p:px -> do
    (decision, st) <- liftIO $ flip runFileMatcher st $
      evalFSNodeTest p >>= iff (fTestDecision test) empty
    maybe (runFTest (test{ fTestPredicate = px }) st action) (action st) decision

-- | Evaluate an 'FSFoldMap' function, returning the final return value paired with the final state
-- value.
runFSFoldMap :: FSFoldMap cr st a -> (Pipe a -> StateT st IO cr) -> st -> IO (cr, st)
runFSFoldMap (FSFoldMap f) = runStateT . runContT f

-- | Same as 'mapDirErr' except 'IOExceptions' that occur while reading the directory are printed to
-- 'stderr' and then ignored.
mapDir
  :: (FPath -> FPath -> FSFoldMap cr st a)
  -> FPath -> FSFoldMap cr st a
mapDir = mapDirErr $ \ parent err ->
  liftIO (hPutStrLn stderr $ show parent ++ ": " ++ show err) >> empty

-- | Map a function to the contents of a directory, that is to say, the names of each of the files
-- in the given target directory. First provide an error handling function in the event that a file
-- could not be read -- feel free to re-throw the exception, the directory file descriptor will be
-- closed properly if you do.
mapDirErr
  :: (FPath -> IOException -> FSFoldMap cr st a)
  -> (FPath -> FPath -> FSFoldMap cr st a) -> FPath -> FSFoldMap cr st a
mapDirErr catcher f dir = do
  stream <- liftIO $ openDirStream $ unpack dir
  let loop = liftIO (try $ readDirStream stream) >>= \ case
        Right file -> if null file then empty else f dir (pack file) <|> loop
        Left   err -> catcher dir err <|> loop
  FSFoldMap $ ContT $ \ next -> StateT $ \ st ->
    runFSFoldMap loop next st `finally` closeDirStream stream

searchLoop :: FTest st a -> FSFoldMapHalt cr st a -> FSearchState st -> FSFoldMap cr st a
searchLoop f halt st = runFTest f st $ \ st result -> yield (theFileMatchYield result) <|> do
  guard $ theFileMatchStepInto result
  maybeGetFileStatus st >>= guard . isDirectory
  flip mapDir (st ^. currentFileNameLens) $ \ _parent path -> searchLoop f halt $
    st{ theCurrentFileName    = path
      , theCurrentSearchPath  = theCurrentFileName st : theCurrentSearchPath st
      , theCurrentFileStatus  = Nothing
      , theCurrentFullPath    = Nothing
      , theCurrentSearchDepth = 1 + theCurrentSearchDepth st
      }

search :: PipeLike pipe => pipe FPath -> FTest st a -> FSFoldMap cr st a
search paths test = callCC $ \ halt -> forM (pipe paths)
  (\ path -> get >>= searchLoop test (FSFoldMapHalt halt) . initFSearchState path) >>= yield
