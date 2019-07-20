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
module Hakshell.Pipe where

import           Prelude hiding (fail)

import           Hakshell.String

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.State hiding (fail)

import           Data.Semigroup

import qualified Data.ByteString.Char8 as BStr
import qualified Data.ByteString.UTF8  as UTF8

----------------------------------------------------------------------------------------------------

type ErrMsg = UTF8.ByteString

-- | /"Ceci n'est pas un pipe."/
--
--   -- Rene Magrite
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
-- modify the type of @a@ by evaluating a function on it with 'fmap'. 'Pipe' is not an 'Applicative'
-- or a 'Monad', which makes it unlike a list. However like list, it does instantiate 'Alternative'
-- and 'MonadPlus', so if you want to sequence items evaluated by 'pull' within a 'Pipe' you can use
-- the ('<|>') operator to append more items onto the content stored within the 'Pipe'. For example:
--
-- @
-- return ('push' 1 <|> 'push' 2 <|> 'push' 3) >>= pull (print . (+ 3))
-- @
--
-- The above will, for each 'push'ed element, add 3 and then print the result.
--
-- Monoid/Semigroup appending is simply lifts the 'mappend' or @('<>')@ operator into 'Pipe'
-- constructor using 'Applicative'. As a consequence, performing 'mappend' om pipes with more than
-- one element doesn't actually append, but computes the multiplicative of the elements. For actual
-- list-like concatenation of the contents of pipes, use '<|>' instead. For example:
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
data Pipe m a
  = PipeStop
  | PipeFail !ErrMsg
  | PipeNext !a (m (Pipe m a))

instance Show (Pipe m a) where
  show = \ case
    PipeStop     -> ""
    PipeFail msg -> "(ERROR "++show (unpack msg)++")"
    PipeNext{}   -> "(OK)"

instance Functor m => Functor (Pipe m) where
  fmap f = \ case
    PipeStop        -> PipeStop
    PipeFail   msg  -> PipeFail msg
    PipeNext a next -> PipeNext (f a) $ fmap (fmap f) next

instance Applicative m => Applicative (Pipe m) where
  pure = flip PipeNext (pure PipeStop)
  (<*>) = \ case
    PipeStop         -> const PipeStop
    PipeFail msg     -> const $ PipeFail msg
    PipeNext f nextF -> \ case
      PipeStop         -> PipeStop
      PipeFail msg     -> PipeFail msg
      PipeNext a nextA -> PipeNext (f a) $ (<*>) <$> nextF <*> pure (PipeNext a nextA)

instance Applicative m => Alternative (Pipe m) where
  empty = PipeStop
  (<|>) = \ case
    PipeStop        -> id
    PipeFail msg    -> const $ PipeFail msg
    PipeNext a next -> PipeNext a . (<$> next) . flip (<|>)

instance Monad m => Monad (Pipe m) where
  return = flip PipeNext (return PipeStop)
  (>>=) = \ case
    PipeStop         -> const PipeStop
    PipeFail msg     -> const $ PipeFail msg
    PipeNext a nextA -> \ m -> case m a of
      PipeStop         -> PipeStop
      PipeFail msg     -> PipeFail msg
      PipeNext b nextB -> PipeNext b $ mplus <$> ((>>= m) <$> nextA) <*> nextB

instance Monad m => MonadPlus (Pipe m) where
  mzero = PipeStop
  mplus = \ case
    PipeStop     -> id
    PipeFail msg -> const $ PipeFail msg
    PipeNext a next -> PipeNext a . (<$> next) . flip mplus

instance Monad m => MonadError ErrMsg (Pipe m) where
  throwError = PipeFail
  catchError try catch = case try of
    PipeStop      -> PipeStop
    PipeFail  msg -> catch msg
    ok@PipeNext{} -> ok

instance Monad m => MonadFail (Pipe m) where
  fail = PipeFail . pack

instance (Applicative m, Semigroup a) => Semigroup (Pipe m a) where
  a <> b = (<>) <$> a <*> b

instance (Applicative m, Monoid a) => Monoid (Pipe m a) where
  mempty = pure mempty
  mappend a b = mappend <$> a <*> b

-- | A pure function to construct a 'Pipe' from a list.
pipe :: Monad m => [a] -> Pipe m a
pipe = foldr ((. pure) . PipeNext) PipeStop

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
pipeEach :: Applicative m => [a] -> (a -> m b) -> m (Pipe m b)
pipeEach ax f = case ax of
  []   -> pure PipeStop
  a:ax -> flip PipeNext (pipeEach ax f) <$> f a

-- | Same as 'foreach' but with the parameters flipped.
mapToPipe :: Applicative m => (a -> m b) -> [a] -> m (Pipe m b)
mapToPipe = flip pipeEach

-- | Perform a single step on the next element of a 'Pipe'. This function automatically handles
-- errors and halting conditions, and serves as a drop-in replacement for any expression that uses a
-- @case@ statement to inspect a 'Pipe'.
--
-- The type signature is the most general form and can be used anywhere a @case@ statement can be
-- used, but most of the time you will use the 'step' function as though it's type signature were:
--
-- @
-- Monad m => (a -> m (Pipe m a) -> f (Pipe m b)) -> Pipe m a -> m (Pipe m b)
-- @
step :: Applicative f => (a -> m1 (Pipe m1 a) -> f (Pipe m2 b)) -> Pipe m1 a -> f (Pipe m2 b)
step f = \ case
  PipeStop        -> pure PipeStop
  PipeFail msg    -> pure $ PipeFail msg
  PipeNext a next -> f a next

-- | Like 'pipeEach' but essentially maps a function to each element in the pipe using the ('<*>')
-- operator.
foreach :: Applicative m => Pipe m a -> (a -> m b) -> m (Pipe m b)
foreach = flip pmap

-- | Same as 'foreach' but with the parameters flipped.
pmap :: Applicative m => (a -> m b) -> Pipe m a -> m (Pipe m b)
pmap f = step $ \ a next -> PipeNext <$> f a <*> (flip foreach f <$> next)

-- | Pull values from the 'Pipe', apply each value to the given continuation. The value returned by
-- continuation is ignored, this function is only for evaluating on functions that produce
-- side-effects.
pull :: (MonadIO m, MonadFail m) => (a -> m void) -> Pipe m a -> m ()
pull f = \ case
  PipeStop        -> return ()
  PipeFail   msg  -> Control.Monad.Except.fail $ BStr.unpack msg
  PipeNext a next -> f a >> next >>= pull f

-- | Pull all values from the 'Pipe' until the pipe finishes.
pullList :: (Monad m, MonadFail m) => (a -> m b) -> Pipe m a -> m [b]
pullList f = loop id where
  loop stack = \ case
    PipeStop        -> return $ stack []
    PipeFail   msg  -> Control.Monad.Except.fail $ BStr.unpack msg
    PipeNext a next -> f a >>= \ b -> next >>= loop (stack . (b :))

-- | Lift the monadic type @inner@ of a 'Pipe' into another monad @m@
liftInnerPipe
  :: (Monad inner, Monad m)
  => (forall b . inner b -> m b) -> Pipe inner a -> m (Pipe m a)
liftInnerPipe lift = step $ \ a next -> return $ PipeNext a $ lift next >>= liftInnerPipe lift

----------------------------------------------------------------------------------------------------

-- | Functions like 'push', 'foreach', and 'pull' are good for simple @IO@ processes. But when your
-- process becomes a little more complicated, it is better to define your @IO@ process in terms of
-- an 'Engine', which manages input, output, an optional state, behind the scenes, allowing you to
-- keep your function definitions clean and compact.
--
-- You define an 'Engine' using the engine combinators such as 'input', 'while', 'collect', and
-- 'output'. You can also use the usual Monad Transformer Library combinators like 'get' and 'put'
-- to update state, the state lenses like 'use' and @('=.')@, error control like 'throwError',
-- 'catchError', and 'Alternative' combinators like @('<|>')@ and 'empty'.
--
-- The name 'Engine' was chosen because it serves as a metaphor for what functions of this type
-- should do: they should take input from a 'Pipe', and produce output to another 'Pipe', typcially
-- performing some work on the content of the input 'Pipe' to produce the output.
newtype Engine st input m a
  = Engine
    { unwrapEngine ::
        StateT (EngineState st input m) m (Pipe (Engine st input m) a)
    }
  deriving (Functor)

data EngineState st input m
  = EngineState
    { theEngineStateValue :: !st
    , theEngineInputPipe  :: (m (Pipe m input))
    }

instance Monad m => Applicative (Engine st input m) where
  pure = Engine . pure . flip PipeNext (Engine $ pure PipeStop)
  (Engine f) <*> (Engine a) = Engine $ (<*>) <$> f <*> a

instance Monad m => Alternative (Engine st input m) where
  empty = Engine $ pure empty
  (Engine a) <|> (Engine b) = Engine $ (<|>) <$> a <*> b

instance Monad m => Monad (Engine st input m) where
  return = Engine . return . return
  (Engine a) >>= f = Engine $ do
    let loop = step $ \ a nextA -> unwrapEngine $ mplus
          (Engine $ unwrapEngine $ f a)
          (Engine $ join <$> unwrapEngine nextA >>= loop)
    a >>= loop 

instance Monad m => MonadPlus (Engine st input m) where
  mzero = Engine $ return mzero
  mplus (Engine a) (Engine b) = Engine $
    a >>= \ a -> b >>= \ b -> return (mplus a b)

instance Monad m => MonadState (EngineState st input m) (Engine st input m) where
  state f = Engine $ state $ \ st0 -> let (a, st) = f st0 in (pure a, st)

instance MonadTrans (Engine st input) where
  lift = Engine . lift . fmap return

instance Monad m => MonadError ErrMsg (Engine st input m) where
  throwError = Engine . return . PipeFail
  catchError (Engine try) catch = Engine $ try >>= \ case
    PipeStop      -> return PipeStop
    PipeFail  msg -> unwrapEngine $ catch msg
    ok@PipeNext{} -> return ok

instance Monad m => MonadFail (Engine st input m) where
  fail = throwError . pack

instance MonadIO m => MonadIO (Engine st input m) where
  liftIO = Engine . fmap pure . liftIO

instance (Monad m, Semigroup a) => Semigroup (Engine st input m a) where
  a <> b = (<>) <$> a <*> b

instance (Monad m, Monoid a) => Monoid (Engine st input m a) where
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
-- 'EngineState'. The engine state may safely be discarded if it is not needed, and so there is also
-- an 'evalEngine' which does discard the resultant 'EngineState'.
--
-- There is also an 'execEngine' function which returns a pipe containing only the state value of
-- type @st@. This can be useful if the 'Engine' function you have written is to behave as an
-- 'unfold'-like function for which the type @output@ is unit @()@ but the state value @st@.
runEngine
  :: forall st input m output
   . Monad m
  => Engine st input m output -> EngineState st input m
  -> m (Pipe m (output, EngineState st input m))
runEngine f init = loop init f where
  loop st f = runStateT (unwrapEngine f) st >>= \ (a, st) -> flip step a $ \ a next ->
    pure (PipeNext (a, st) $ loop st $ Engine $ fmap join $ unwrapEngine next)

-- | Like 'runEngine' but discards the 'EngineState', leaving a 'Pipe' with only the values of type
-- @output@. Use this function when the intermediate state values of type @st@ are not important,
-- and only the 'Pipe'ed @output@ is important.
evalEngine :: Monad m => Engine st input m output -> EngineState st input m -> m (Pipe m output)
evalEngine f = fmap (fmap fst) . runEngine f

-- | Like 'runEngine' but discards the values of type @output@, returning a 'Pipe' containing every
-- intermediate state value of type @st@. This can be useful if the 'Engine' function you have
-- written is to behave as an 'unfold'-like function for which the type @output@ is unit @()@ but
-- the state value @st@.
execEngine :: Monad m => Engine st input m output -> EngineState st input m -> m (Pipe m st)
execEngine f = fmap (fmap (theEngineStateValue . snd)) . runEngine f

-- | This is a combinator to define a function of type 'Engine'. When this function is evaluated, it
-- take a single value of type @input@ from the input stream that is piped to every 'Engine'
-- function when it is evaluated by 'evalEngine'.
input :: Monad m => Engine st input m input
input = Engine $ use engineInputPipe >>= lift >>=
  step (\ input next -> engineInputPipe .= next >> return (pure input))

-- | This function is similar to 'pushList', but is specifically designed to evaluate within a
-- function of type 'Engine'. As the name implies, this function is a counterpart to the 'input'
-- function in that after the 'Engine' has been run with 'evalEngine', the elements evaluated by
-- 'output' can be /piped/ to the 'input' of another function evaluated by 'evalEngine' using the
-- monadic bind operator @'>>='@ as the piping operator, and whatever is 'ouput' by this function
-- can be received on the other end by the 'input' function.
output :: Monad m => [output] -> Engine st input m output
output = pushList >=> Engine . return

-- | This function serves as a form of 'Data.List.unfold'ing function, it is a little similar to the
-- UNIX "@yes@" function. It works by evaluating a given 'Engine' function repeatedly in an
-- infinitely recursive loop while collecting the @output@ from evaluation. The loop continues until
-- 'empty' or 'mzero' is evaluated (which are identical functions in standard Haskell). Note that
-- evaluating the 'input' function to obtain an input will evaluate to 'empty' as well, if the
-- 'Engine' you pass to 'pump' evaluates 'input' every time, the 'pump' will keep looping until all
-- input is consumed -- this is how the 'mapInput' function is defined.
--
-- The 'Data.List.unfold'ing comes from the fact that you can use the 'get', 'put', 'modify', and
-- 'state' functions from the "Control.Monad.State" module to repeatedly update a stateful value
-- from which your next @output@ value will derive.
--
-- Note that the given 'Engine' function which is to be looped must be evaluated at least one time
-- so that the @output@ value can be inspected and a decision can be made as to whether the loop
-- should continue, however if the first evaluation of 'while' is 'empty' then no 'output' is ever
-- produced, so that first evaluation may not have any meaningful side-effects.
pump :: Monad m => Engine st input m output -> Engine st input m output
pump (Engine f) = Engine $ f >>=
  step (\ input next -> unwrapEngine $ Engine (pure $ PipeNext input next) <|> pump (Engine f))

-- | This function is similar to 'pump', except the 'input' function is evaluated on each iteration,
-- and the result of the input is fed into the given 'Engine' function.
mapInput :: Monad m => (input -> Engine st input m output) -> Engine st input m output
mapInput = pump . (input >>=)

-- | You should never need to use this function.
--
-- This function used is internally to define the 'Engine' combinators which take elements from the
-- input pipe. It operates on a value of the concrete 'EngineState' type and not the variable type
-- @st@, meaning you cannot evaluate lens expressions such as 'use' or @('.=')@ or @('%=')@ unless
-- you wrap these expressions in the 'Engine' constructor first. For example the 'putBackInput'
-- function is defined as: @\\ elem -> 'Engine' ('engineInputPipe' '%=' 'PipeNext' elem . 'return')@
--
-- Again, just use the input combinators like 'input', 'collect', 'while', and 'putBackInput'.
engineInputPipe :: Lens' (EngineState st input m) (m (Pipe m input))
engineInputPipe = lens theEngineInputPipe $ \ a b -> a{ theEngineInputPipe = b }

-- | You should never need to use this function.
--
-- This function is used to define the instnaces of 'get' and 'put' (of the 'MonadState' typeclass)
-- for the 'Engine' function type.
--
-- @
-- 'put' = 'Engine' '.' 'assign' 'engineStateValue'
-- 'get' = Engine '$' 'use' 'engineStateValue'
-- @
engineStateValue :: Lens' (EngineState st input m) st
engineStateValue = lens theEngineStateValue $ \ a b -> a{ theEngineStateValue = b }
