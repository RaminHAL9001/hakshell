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
module Hakshell.Pipe
  ( module Hakshell.Pipe
  , module Control.Applicative
  , module Data.Foldable
  , module Data.Semigroup
  , module Data.Traversable
  , Control.Monad.join
  , Control.Monad.forever
  , Control.Monad.mzero
  , Control.Monad.mplus
  , Control.Monad.guard
  , Control.Monad.replicateM
  ) where

import           Prelude hiding (fail)

import           Hakshell.String

import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad       hiding (mapM, mapM_, forM, forM_)
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.State hiding (fail)

import           Data.Foldable
import           Data.Semigroup
import qualified Data.Sequence         as Seq
import           Data.Traversable

import qualified Data.ByteString.UTF8  as UTF8

----------------------------------------------------------------------------------------------------

-- | When writing pipelines using Continuation Passing Style (CPS), you sometimes want to start with
-- a pure argument value, for example the 'search' function takes a list of directories to search as
-- it's final argument, and a predicate and action as it's first two arguments:
--
-- @
-- 'values' [".\/here", ".\/there"] $ 'Hakshell.Find.search' (['Hakshell.Find.file' 'Data.Semigroup.<>' 'Hakshell.Find.isNamed' "index.html"] ?-> 'Hakshell.Find.matchPrune') 'Control.Applicative.pure'
-- @
--
-- would be equivalent to writing:
--
-- @
-- 'Hakshell.Find.search' (['Hakshell.Find.file' 'Data.Semigroup.<>' 'Hakshell.Find.isNamed' "index.html"] ?-> 'Hakshell.Find.matchPrune') 'Control.Applicative.pure' [".\/here", ".\/there"]
-- @
--
-- This seems useless at first, until you decide you want to replace the 'Control.Applicative.pure'
-- function with an inline/anonymous filter function:
--
-- @
-- 'Hakshell.Find.search' (['Hakshell.Find.file' 'Data.Semigroup.<>' 'Hakshell.Find.isNamed' "index.html"] ?-> 'Hakshell.Find.matchPrune') (some >=> complicated >=> filtering >=> criteria)
--     [".\/here", ".\/there"]
-- @
--
-- In this case, it is a little more convenient to use 'values', which allows you to treat functions
-- between the 'Prelude.$' operators more like pipes in a pipeline.
--
-- @
-- 'values' [".\/here", ".\/there"]
--   $ 'Hakshell.Find.search' (['Hakshell.Find.file' 'Data.Semigroup.<>' 'Hakshell.Find.isNamed' "index.html"] ?-> 'Hakshell.Find.matchPrune')
--   $ some >=> complicated >=> filtering >=> criteria
-- @
--
-- Writing the code this way allows you to start with your values, then "pipe" these values to the
-- 'search' function, then "pipe" the files to some filtering criteria, all using the 'Prelude.$'
-- operator.
values :: a -> (a -> b) -> b
values = flip ($)

----------------------------------------------------------------------------------------------------

-- | /"Ceci n'est pas un pipe."/  -- Rene Magrite
--
-- 'Pipe' is a data type used to approximate the behavior of UNIX pipes.
--
-- You use the 'pull' function to evaluate an 'Applicative' function of your choice on the content
-- within the 'Pipe'. The 'Pipe' is a "Mealy Machine" data type, meaning when 'pull' is evaluated,
-- it pattern match on the 'Pipe' to extract a value to be applied to your 'Applicative' function,
-- and then another applicative function is also extracted and evaluated to produce another 'Pipe',
-- and each successive 'Pipe' is recursively evaluated.
--
-- 'Pipe' instantiates the 'Show' class to show a status:
--
-- * @(OK)@ on the 'PipeNext' constructor, means you can begin extracting values using 'pull'.
--
-- * @(ERROR "some message")@ means an error occurred.
--
-- * An empty string indicates the 'PipeStop' constructor was returned, which means no error
--   ocurred, and no result was returned either.
--
-- Construct a 'Pipe' using functions like 'push' and 'foreach'. Pipe is also a functor, so you can
-- modify the type of @a@ by evaluating a function on it with 'fmap'. 'Pipe' is not a 'Monad', so it
-- is unlike a list 'Control.Monad.List.ListT' data type. However a 'Pipe' is an instance of the
-- 'Applicative' and 'Alternative' typeclasses, so if you want to sequence items evaluated by 'pull'
-- within a 'Pipe' you can use the ('<|>') operator to append more items onto the content stored
-- within the 'Pipe'. For example:
--
-- @
-- return ('push' 1 <|> 'push' 2 <|> 'push' 3) >>= pull (print . (+ 3))
-- @
--
-- The above will, for each 'push'ed element, add 3 and then print the result.
--
-- To use 'Pipe's in a monadic context, use the 'EngineT' function type, which uses 'Pipe's as a
-- control mechanism but also instantiates 'Monad', 'MonadPlus', 'MonadState' (for an arbitrary
-- state value), 'MonadTrans', and 'MonadIO'.
--
-- Monoid/Semigroup appending for both 'Pipe' and 'EngineT' is simply to lift the 'mappend' or
-- @('<>')@ operator into 'Pipe' constructor using 'Applicative'. As a consequence, performing
-- 'mappend' om pipes with more than one element doesn't actually append, but computes the
-- multiplicative of the elements. For actual list-like concatenation of the contents of pipes, use
-- '<|>' instead. For example:
--
-- @
-- ('push' "A" '<|>' 'push' "B" '<|>' 'push' "C") '<>' ('push' "X" <|> 'push' "Y" <|> 'push' "Z")
-- @
--
-- ...would construct a 'Pipe' containing elements equivalent to:
--
-- @
-- 'pipeList' ["AX", "AY", "AZ", "BX", "BY", "BZ", "CX", "CY", "CZ"]
-- @
--
-- Notice that 'Semigroup'/'Monoid' concatenation is similar to arithmetic in how you would compute
-- the expression @(a + b + c) * (x + y + z)@ using the distributive property of multiplication.
newtype Pipe a = Pipe{ unwrapPipe :: Seq.Seq a }
  deriving (Functor)

instance Show (Pipe a) where
  show = const "(OK)"

instance Applicative Pipe where
  pure = Pipe . pure
  (<*>) (Pipe f) (Pipe a) = Pipe $ f <*> a 

instance Alternative Pipe where
  empty = Pipe empty
  (<|>) (Pipe a) (Pipe b) = Pipe $ a <|> b

instance Monad Pipe where
  return = Pipe . return
  (>>=) (Pipe a) f = Pipe $ a >>= (\ (Pipe f) -> f) . f

instance Foldable Pipe where
  foldMap f (Pipe a) = foldMap f a
  foldr f b (Pipe a) = foldr f b a

instance Traversable Pipe where
  traverse f (Pipe a) = Pipe <$> traverse f a
  sequenceA (Pipe a) = Pipe <$> sequenceA a

instance Semigroup a => Semigroup (Pipe a) where
  a <> b = (<>) <$> a <*> b

instance Monoid a => Monoid (Pipe a) where
  mempty = empty
  mappend a b = mappend <$> a <*> b

class PipeLike thing where { pipe :: thing a -> Pipe a; }
instance PipeLike [] where { pipe = Pipe . Seq.fromList; }
instance PipeLike Seq.Seq where { pipe = Pipe; }
instance PipeLike Pipe where { pipe = id; }

-- | Return a 'Pipe' containing a single value.
push :: Applicative m => a -> m (Pipe a)
push = pure . pure

-- | Return a 'Pipe' containing zero or more values.
pushList :: Applicative m => [a] -> m (Pipe a)
pushList = pure . Pipe . Seq.fromList

-- | Similar to 'forM', but evaluates a function on each element of a list, and each item is
-- 'yield'ed in turn.
foreach :: (Applicative m, PipeLike pipe) => pipe a -> (a -> m b) -> m (Pipe b)
foreach = flip mapToPipe

-- | Same as 'foreach' but with the parameters flipped.
mapToPipe :: (Applicative m, PipeLike pipe) => (a -> m b) -> pipe a -> m (Pipe b)
mapToPipe f = traverse f . pipe

-- | Perform a single step on the next element of a 'Pipe'. This function automatically handles
-- errors and halting conditions, and serves as a drop-in replacement for any expression that uses a
-- @case@ statement to inspect a 'Pipe'.
--
-- The type signature is the most general form and can be used anywhere a @case@ statement can be
-- used, but most of the time you will use the 'step' function as though it's type signature were:
--
-- @
-- Monad m => (a -> m (Pipe m a) -> m (Pipe m b)) -> Pipe m a -> m (Pipe m b)
-- @
step :: Applicative m => (a -> Pipe a -> m (Pipe b)) -> Pipe a -> m (Pipe b)
step f (Pipe a) = case a of
  Seq.Empty        -> pure empty
  (a Seq.:<| next) -> f a $ Pipe next

----------------------------------------------------------------------------------------------------

-- | A class of monads that can yield multiple values from a 'Pipe' without having to deconstruct it
-- to a list and then reconstruct it using @('msum' . 'fmap' 'pure')@.
class Monad m => MonadPipe m where { yield :: Pipe a -> m a; }

----------------------------------------------------------------------------------------------------

-- | Functions like 'push', 'foreach', and 'pull' are good for simple @IO@ processes. But when your
-- process becomes a little more complicated, it is better to define your @IO@ process in terms of
-- an 'EngineT', which manages input, output, an optional state, behind the scenes, allowing you to
-- keep your function definitions clean and compact. You would also use 'EngineT' in the case that
-- you need monadic behavior from a 'Pipe', which can't be done with 'Pipe' alone since 'Pipe' is
-- not a monad (although it is an 'Applicative' 'Functor').
--
-- You define an 'EngineT' using the 'EngineT' combinators such as 'input', 'pump', 'while',
-- 'collect', and 'output'. You can convert an ordinary 'Pipe' to an 'EngineT' by evaluating the
-- 'Pipe' with the 'EngineT' function. You can also use the usual Monad Transformer Library
-- combinators like 'get' and 'put' to update state, the state lenses like 'use' and @('=.')@, error
-- control like 'throwError', 'catchError', and 'Alternative' combinators like @('<|>')@ and
-- 'empty'.
--
-- The name 'EngineT' was chosen because it serves as a metaphor for what functions of this type
-- should do: they should take input from a 'Pipe', and produce output to another 'Pipe', typcially
-- performing some work on the content of the input 'Pipe' to produce the output.
newtype EngineT st input m a
  = EngineT{ unwrapEngineT :: ExceptT EngineError (StateT (EngineState st input) m) (Pipe a) }
  deriving (Functor)

type Engine st input = EngineT st input Identity

data EngineError
  = EngineFail UTF8.ByteString
  | EngineIOError IOException
  deriving (Eq, Show)

data EngineState st input
  = EngineState
    { theEngineStateValue :: !st
    , theEngineInputPipe  :: Pipe input
    }

instance Packable EngineError where
  pack = EngineFail . pack

instance Semigroup st => Semigroup (EngineState st input) where
  a <> b = EngineState
    { theEngineStateValue = theEngineStateValue a <> theEngineStateValue b
    , theEngineInputPipe  = theEngineInputPipe a <|> theEngineInputPipe b
    }

instance Monoid st => Monoid (EngineState st input) where
  mempty = EngineState
    { theEngineStateValue = mempty
    , theEngineInputPipe  = empty
    }
  mappend a b = EngineState
    { theEngineStateValue = theEngineStateValue a `mappend` theEngineStateValue b
    , theEngineInputPipe  = theEngineInputPipe a <|> theEngineInputPipe b
    }

instance Monad m => Applicative (EngineT st input m) where
  pure = EngineT . pure . pure
  (EngineT f) <*> (EngineT a) = EngineT $ (<*>) <$> f <*> a

instance Monad m => Alternative (EngineT st input m) where
  empty = EngineT $ pure empty
  (EngineT a) <|> (EngineT b) = EngineT $ (<|>) <$> a <*> b

instance Monad m => Monad (EngineT st input m) where
  return = EngineT . return . pure
  (EngineT a) >>= f = EngineT $ fmap (unwrapEngineT . f) <$> a >>= fmap join . sequence

instance Monad m => MonadPlus (EngineT st input m) where { mzero = empty; mplus = (<|>); }

instance Monad m => MonadState (EngineState st input) (EngineT st input m) where
  state f = EngineT $ state $ \ st0 -> let (a, st) = f st0 in (pure a, st)

instance MonadTrans (EngineT st input) where
  lift = EngineT . lift . lift . fmap pure

instance Monad m => MonadError EngineError (EngineT st input m) where
  throwError = EngineT . throwError
  catchError (EngineT try) = EngineT . catchError try . (unwrapEngineT .)

instance Monad m => MonadFail (EngineT st input m) where
  fail = throwError . pack

instance MonadIO m => MonadIO (EngineT st input m) where
  liftIO = EngineT . liftIO . fmap pure

instance (Monad m, Semigroup a) => Semigroup (EngineT st input m a) where
  a <> b = (<>) <$> a <*> b

instance (Monad m, Monoid a) => Monoid (EngineT st input m a) where
  mappend a b = mappend <$> a <*> b
  mempty      = return mempty

-- | The 'runEngine' function can behave as both a 'Data.Foldable.foldl' function, and also an
-- 'Data.List.unfoldr' function. Notice that the parameters to this function are very similar to the
-- 'Data.Foldable.foldl' function, with:
--
-- 1. The folding function, expressed as a function of type 'Engine'
--
-- And then the 'EngineState' value which contains:
--
-- 2. The arbitrary folding value, in this case denoted as a value of variable type @st@.
--
-- 3. The traversable (list-like) value, in this case a 'Pipe'.
--
-- ...and parameters 1, 2, and 3 above, given in that order, is just like 'Data.Foldable.foldl'.
--
-- 'Engine' Functions can also be defined to operate in a way similar to the 'Data.List.unfoldr'
-- function, by defining a recursive function which updates the @st@ value and derives an @output@
-- value from the @st@ after each update (the @output@ value may, at times, also be the same type as
-- the @st@ value). The value of type @st@ can be accessed with the "Control.Monad.State" functions
-- 'Control.Monad.State.get' and 'Control.Monad.State.put', yield an 'output' simply by 'return'ing
-- it.
--
-- This function also receives an input pipe which may be used or not, and you can retrieve as many
-- or as few pipe elements from the input as the algorithm you are writing requires.
--
-- The output of this function is a pipe that produces a result of type @output@ paired with the
-- 'EngineState'. The EngineT state may safely be discarded if it is not needed, and so there is also
-- an 'evalEngine' which does discard the resultant 'EngineState'.
--
-- There is also an 'execEngine' function which returns a pipe containing only the state value of
-- type @st@. This can be useful if the 'Engine' function you have written is to behave as an
-- 'unfold'-like function for which the type @output@ is unit @()@ but the state value @st@.
runEngineT
  :: Monad m
  => EngineT st input m output -> EngineState st input
  -> m (Either EngineError (Pipe output), EngineState st input)
runEngineT = runStateT . runExceptT . unwrapEngineT

-- | The pure version of 'runEngineT'.
runEngine
  :: Engine st input output
  -> EngineState st input
  -> (Either EngineError (Pipe output), EngineState st input)
runEngine = (.) runIdentity . runEngineT

-- | Like 'runEngine' but discards the 'EngineState', leaving a 'Pipe' with only the values of type
-- @output@. Use this function when the intermediate state values of type @st@ are not important,
-- and only the 'Pipe'ed @output@ is important.
evalEngineT
  :: Monad m
  => EngineT st input m output -> EngineState st input
  -> m (Either EngineError (Pipe output))
evalEngineT f = fmap fst . runEngineT f

-- | The pure version of 'evalEngineT'.
evalEngine :: Engine st input output -> EngineState st input -> Either EngineError (Pipe output)
evalEngine = (.) runIdentity . evalEngineT

-- | Like 'runEngine' but discards the values of type @output@, returning a 'Pipe' containing every
-- intermediate state value of type @st@. This can be useful if the 'Engine' function you have
-- written is to behave as an 'unfold'-like function for which the type @output@ is unit @()@ but
-- the state value @st@.
execEngineT
  :: Monad m
  => EngineT st input m output -> EngineState st input
  -> m (EngineState st input)
execEngineT = fmap (fmap snd) . runEngineT

execEngine :: Engine st input output -> EngineState st input -> EngineState st input
execEngine = (.) runIdentity . execEngineT

instance Monad m => MonadPipe (EngineT st input m) where
  yield = EngineT . pure

-- | Throw an 'IOException' in the 'EngineT' monad.
engineIOError :: MonadIO m => IOException -> EngineT st input m void
engineIOError = throwError . EngineIOError

-- | This is a combinator to define a function of type 'Engine'. When this function is evaluated, it
-- take a single value of type @input@ from the input stream that is piped to every 'Engine'
-- function when it is evaluated by 'evalEngine'.
input :: Monad m => EngineT st input m input
input = EngineT $ use engineInputPipe >>=
  step (\ input next -> engineInputPipe .= next >> push input)

-- | This function is similar to 'pushList', but is specifically designed to evaluate within a
-- function of type 'Engine'. As the name implies, this function is a counterpart to the 'input'
-- function in that after the 'Engine' has been run with 'evalEngine', the elements evaluated by
-- 'output' can be /piped/ to the 'input' of another function evaluated by 'evalEngine' using the
-- monadic bind operator @'>>='@ as the piping operator, and whatever is 'ouput' by this function
-- can be received on the other end by the 'input' function.
output :: Monad m => [output] -> EngineT st input m output
output = pushList >=> EngineT . return

-- | This function performs what you could call a 'map'ping function on a 'Pipe'. This function
-- loops infinitely on a procedural function of type @'Engine' st inp m outp@, calling 'input' for
-- you at the start of each each loop iteration and passing that input to the given
-- procedure. Iteration continues until all 'input's have been consumed, even if the procedure
-- evaluates to 'empty'. If the 'procedure' evaluates to 'empty' (or equivalently 'guard' is given a
-- 'False' value), then no output is produced for that iteration, and the loop begins again with the
-- next iteration if there are 'input's remaining.
while :: Monad m => (input -> EngineT st input m output) -> EngineT st input m output
while f = input >>= (<|> (while f)) . f

-- | You should never need to use this function.
--
-- This function used is internally to define the 'Engine' combinators which take elements from the
-- input pipe. It operates on a value of the concrete 'EngineState' type and not the variable type
-- @st@, meaning you cannot evaluate lens expressions such as 'use' or @('.=')@ or @('%=')@ unless
-- you wrap these expressions in the 'Engine' constructor first. For example the 'putBackInput'
-- function is defined as: @\\ elem -> 'Engine' ('engineInputPipe' '%=' 'PipeNext' elem . 'return')@
--
-- Again, just use the input combinators like 'input', 'collect', 'pump', and 'putBackInput'.
engineInputPipe :: Lens' (EngineState st input) (Pipe input)
engineInputPipe = lens theEngineInputPipe $ \ a b -> a{ theEngineInputPipe = b }

-- | You should never need to use this function.
--
-- This function is used to define the instnaces of 'get' and 'put' (of the 'MonadState' typeclass)
-- for the 'Engine' function type.
--
-- @
-- 'put' = 'EngineT' '.' 'assign' 'engineStateValue'
-- 'get' = EngineT '$' 'use' 'engineStateValue'
-- @
engineStateValue :: Lens' (EngineState st input) st
engineStateValue = lens theEngineStateValue $ \ a b -> a{ theEngineStateValue = b }
