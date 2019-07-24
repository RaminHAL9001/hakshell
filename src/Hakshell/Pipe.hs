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
-- state value), 'MonadFail' and 'MonadError' (for throwing and catching 'PipeFail' values),
-- 'MonadTrans', and 'MonadIO'.
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
    PipeNext a next -> PipeNext (f a) $ fmap f <$> next

instance Applicative m => Applicative (Pipe m) where
  pure = flip PipeNext (pure PipeStop)
  (<*>) = \ case
    PipeStop         -> const PipeStop
    PipeFail msg     -> const $ PipeFail msg
    PipeNext f nextF -> \ case
      PipeStop         -> PipeStop
      PipeFail msg     -> PipeFail msg
      PipeNext a nextA -> PipeNext (f a) $ (<|>) <$> (fmap f <$> nextA) <*>
        ((<*> (PipeNext a nextA)) <$> nextF)

instance Applicative m => Alternative (Pipe m) where
  empty = PipeStop
  (<|>) = \ case
    PipeStop        -> id
    PipeFail msg    -> const $ PipeFail msg
    PipeNext a next -> PipeNext a . (<$> next) . flip (<|>)

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
-- Monad m => (a -> m (Pipe m a) -> m (Pipe m b)) -> Pipe m a -> m (Pipe m b)
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
pmap f = step $ \ a next -> PipeNext <$> f a <*> (pmap f <$> next)

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
-- an 'EngineT', which manages input, output, an optional state, behind the scenes, allowing you to
-- keep your function definitions clean and compact. You would also use 'EngineT' in the case that
-- you need monadic behavior from a 'Pipe', which can't be done with 'Pipe' alone since 'Pipe' is
-- not a monad (although it is an 'Applicative' 'Functor').
--
-- You define an 'EngineT' using the 'EngineT' combinators such as 'input', 'while', 'collect', and
-- 'output'. You can convert an ordinary 'Pipe' to an 'EngineT' by evaluating the 'Pipe' with the
-- 'EngineT' function. You can also use the usual Monad Transformer Library combinators like 'get'
-- and 'put' to update state, the state lenses like 'use' and @('=.')@, error control like
-- 'throwError', 'catchError', and 'Alternative' combinators like @('<|>')@ and 'empty'.
--
-- The name 'EngineT' was chosen because it serves as a metaphor for what functions of this type
-- should do: they should take input from a 'Pipe', and produce output to another 'Pipe', typcially
-- performing some work on the content of the input 'Pipe' to produce the output.
newtype EngineT st input m a
  = EngineT
    { unwrapEngineT ::
        StateT (EngineState st input m) m (Pipe (EngineT st input m) a)
    }
  deriving (Functor)

type Engine st input = EngineT st input Identity

data EngineState st input m
  = EngineState
    { theEngineStateValue :: !st
    , theEngineInputPipe  :: (m (Pipe m input))
    }

instance Monad m => Applicative (EngineT st input m) where
  pure = EngineT . pure . flip PipeNext (EngineT $ pure PipeStop)
  (EngineT f) <*> (EngineT a) = EngineT $ (<*>) <$> f <*> a

instance Monad m => Alternative (EngineT st input m) where
  empty = EngineT $ pure empty
  (EngineT a) <|> (EngineT b) = EngineT $ (<|>) <$> a <*> b

instance Monad m => Monad (EngineT st input m) where
  return = EngineT . return . pure
  (EngineT a) >>= f = EngineT $
    let loop = step $ \ a next ->
          (<|>) <$> unwrapEngineT (f a) <*> unwrapEngineT (next >>= EngineT . loop)
    in  a >>= loop

instance Monad m => MonadPlus (EngineT st input m) where { mzero = empty; mplus = (<|>); }

instance Monad m => MonadState (EngineState st input m) (EngineT st input m) where
  state f = EngineT $ state $ \ st0 -> let (a, st) = f st0 in (pure a, st)

instance MonadTrans (EngineT st input) where
  lift = EngineT . lift . fmap pure

instance Monad m => MonadError ErrMsg (EngineT st input m) where
  throwError = EngineT . return . PipeFail
  catchError (EngineT try) catch = EngineT $ try >>= \ case
    PipeStop      -> return PipeStop
    PipeFail  msg -> unwrapEngineT $ catch msg
    ok@PipeNext{} -> return ok

instance Monad m => MonadFail (EngineT st input m) where
  fail = throwError . pack

instance MonadIO m => MonadIO (EngineT st input m) where
  liftIO = EngineT . fmap pure . liftIO

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
  => EngineT st input m output -> EngineState st input m
  -> m (Pipe m (output, EngineState st input m))
runEngineT f = fmap
  ( \ (a, st) -> case a of
    PipeStop        -> PipeStop
    PipeFail err    -> PipeFail err
    PipeNext a next -> PipeNext (a, st) $ runEngineT (next >>= EngineT . pure) st
  ) . runStateT (unwrapEngineT f)

-- | The pure version of 'runEngineT'.
runEngine
  :: Engine st input output
  -> EngineState st input Identity
  -> Pipe Identity (output, EngineState st input Identity)
runEngine f = runIdentity . runEngineT f

-- | Like 'runEngine' but discards the 'EngineState', leaving a 'Pipe' with only the values of type
-- @output@. Use this function when the intermediate state values of type @st@ are not important,
-- and only the 'Pipe'ed @output@ is important.
evalEngineT :: Monad m => EngineT st input m output -> EngineState st input m -> m (Pipe m output)
evalEngineT f = fmap (fmap fst) . runEngineT f

-- | The pure version of 'evalEngineT'.
evalEngine :: Engine st input output -> EngineState st input Identity -> Pipe Identity output
evalEngine f = runIdentity . evalEngineT f

-- | Like 'runEngine' but discards the values of type @output@, returning a 'Pipe' containing every
-- intermediate state value of type @st@. This can be useful if the 'Engine' function you have
-- written is to behave as an 'unfold'-like function for which the type @output@ is unit @()@ but
-- the state value @st@.
execEngineT :: Monad m => EngineT st input m output -> EngineState st input m -> m (Pipe m st)
execEngineT f = fmap (fmap (theEngineStateValue . snd)) . runEngineT f

execEngine :: Engine st input output -> EngineState st input Identity -> Pipe Identity st
execEngine f = runIdentity . execEngineT f

-- | Convert a plain 'Pipe' into an 'EngineT'.
engine :: Monad m => Pipe m a -> EngineT st input m a
engine = \ case
  PipeStop        -> empty
  PipeFail err    -> throwError err
  PipeNext a next -> EngineT $ ((pure a) <|>) <$> (lift next >>= unwrapEngineT . engine)

-- | This is a combinator to define a function of type 'Engine'. When this function is evaluated, it
-- take a single value of type @input@ from the input stream that is piped to every 'Engine'
-- function when it is evaluated by 'evalEngine'.
input :: Monad m => EngineT st input m input
input = EngineT $ use engineInputPipe >>= lift >>=
  step (\ input next -> engineInputPipe .= next >> return (pure input))

-- | This function is similar to 'pushList', but is specifically designed to evaluate within a
-- function of type 'Engine'. As the name implies, this function is a counterpart to the 'input'
-- function in that after the 'Engine' has been run with 'evalEngine', the elements evaluated by
-- 'output' can be /piped/ to the 'input' of another function evaluated by 'evalEngine' using the
-- monadic bind operator @'>>='@ as the piping operator, and whatever is 'ouput' by this function
-- can be received on the other end by the 'input' function.
output :: Monad m => [output] -> EngineT st input m output
output = pushList >=> EngineT . return

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
pump :: Monad m => EngineT st input m output -> EngineT st input m output
pump (EngineT f) = EngineT $ f >>=
  step (\ input next -> unwrapEngineT $ EngineT (pure $ PipeNext input next) <|> pump (EngineT f))

-- | This function is similar to 'pump', except the 'input' function is evaluated on each iteration,
-- and the result of the input is fed into the given 'Engine' function.
mapInput :: Monad m => (input -> EngineT st input m output) -> EngineT st input m output
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
-- 'put' = 'EngineT' '.' 'assign' 'engineStateValue'
-- 'get' = EngineT '$' 'use' 'engineStateValue'
-- @
engineStateValue :: Lens' (EngineState st input m) st
engineStateValue = lens theEngineStateValue $ \ a b -> a{ theEngineStateValue = b }
