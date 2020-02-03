module Hakshell.TextEditor.FoldMap
  (
    -- * Batch Editing

    -- ** A Function Type for Both Folding and Mapping

    FoldMapLines, FoldMapLinesHalt, 

    -- *** Rewriting Lines

    RewriteLines, rewriteLines, rewriteLinesInRange, rewriteLinesInBuffer,

    -- *** Only mapping over lines  of text
    --
    -- Folding and mapping can both be done in a single pass. It is also possible to halt a
    -- folding/mapping function by evaluating a halting continuation function provided by
    -- 'forLinesInRange', 'forLines', and 'forLinesInBuffer'. Functions of the type described here
    -- are used to perform statelses updates on a buffer, for example a context-free search and
    -- replace function.

    MapLines, mapLines, mapLinesInRange, mapLinesInBuffer,
    forLines, forLinesInRange, forLinesInBuffer,

    -- *** Only folding over lines of text
    --
    -- These functions perform a batch read-only opertion over the 'TextBuffer' without moving the
    -- position of the cursor. Be careful to evaluate 'flushLineEditor' before evaluating folds over
    -- 'TextBuffer's to ensure the latest changes to the 'LineEditor' are actually stored into the
    -- 'TextBuffer' and are ready to be folded, or your results may not be what you expect.

    FoldLines, foldLines, foldLinesInRange, foldLinesInBuffer,

    -- *** Evaluate folds or maps ('FoldMapLines' functions) without looping
    --
    -- These functions do not iterate over a range of lines in the buffer, rather they evaluate a
    -- function of type 'FoldMapLines' just once, which reduces it to a function of type 'EditText'.
    -- These are not batch operations, they only remove the outer-most monad of the 'MapLines' and
    -- 'FoldMapLines' function types. To do batch operations, use 'forLines' instead.

    runFoldMapLinesStep, execFoldMapLinesStep, evalFoldMapLinesStep,
    runFoldLinesStep, runMapLinesStep,

    -- ** Folding and mapping over characters

    FoldMapChars, foldMapChars, runFoldMapChars,

    -- ** Mapping over characters in a line of text
    --
    -- The 'MapChars' function type is a special case of 'FoldMapChars'.

    MapChars, runMapChars,

    -- *** Evaluate a 'FoldMapChars' function without input
    --
    -- These functions do not iterate over a range of characters in the buffer, rather they evaluate
    -- a function of type 'FoldMapChars' just once, which reduces it to a function of type
    -- 'EditLine'.

    execFoldMapChars, evalFoldMapChars,

    -- ** Text Editing Typeclasses
    --
    -- These type classes are defined so that some of the 'EditText' type of functions can be
    -- evaluated within a batch editing type of function witouht having to supply a lifting
    -- function.

    MonadEditText(..), MonadEditLine(..),
  ) where

import           Hakshell.TextEditor

import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.State

----------------------------------------------------------------------------------------------------

-- | A type of functions that can perform a fold (in the sense of the Haskell 'Control.Monad.foldM'
-- function) over lines of text in a 'TextBufferState'. This function takes an arbitrary @fold@ data
-- type which can be anything you want, and is initialized when evaluating the 'runFoldMapLines'
-- function. The 'FoldMapLines' function type instantiates 'Control.Monad.State.Class.MonadState'
-- over the @fold@ type, so you will use 'Control.Monad.State.state', 'Control.Monad.State.modify',
-- 'Control.Monad.State.get', and 'Control.Monad.State.Put' functions
--
-- Not that the term "fold" as it is used in this function's name is not to be confused with "fold"
-- as in folding paper. "Folding" is a term that many graphical text editors use to describe a
-- feature in which a block of contiguous lines of text can be hidden (not displayed on screen), as
-- if the text buffer were a piece of paper and the paper was folded twice, once above the start of
-- the first line of the block of text, and once below the bottom line of the block of text, then
-- pushing the edges of the folds of paper together to obscure the text between the folds. The
-- 'FoldMapLines' has nothing to do with such a feature.
newtype FoldMapLines r fold tags m a
  = FoldMapLines
    { unwrapFoldMapLines :: ContT r (ExceptT TextEditError (StateT fold (EditText tags m))) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | When evaluating 'forLinesInRange', a 'FoldMapLines' function is evaluated. The 'FoldMapLines'
-- function type instantiates the 'Control.Monad.Cont.Class.MonadCont' type class, and the
-- 'Control.Monad.Class.callCC' function is evaluated before running the fold map operation,
-- producing a halting function. The halting function is of this data type.
--
-- Suppose you would like to fold and map over lines 5 through 35 counting the lines as you go, but
-- halt if the line of text is @"stop\\n"@, you would evaluate 'forLinesInRange' like so:
--
-- @
-- stopSymbol <- 'Data.List.head' 'Control.Applicative.<$>' 'textLines' "stop\n"
-- 'foldMapLines' 5 35 $ \\ halt thisLine -> do
--     count <- 'Control.Monad.State.Class.get'
--     'Control.Monad.when' (thisLine == stopSymbol) (halt count)
--     -- If the "halt" function was evaluated in the above "when" statement,
--     -- then the code below will not be evaluated.
--     'Control.Monad.State.Class.put' (count + 1)
--     return [thisLine]
-- @
type FoldMapLinesHalt void fold tags m r = r -> FoldMapLines r fold tags m void

instance Monad m => MonadState fold (FoldMapLines r fold tags m) where
  state = FoldMapLines . lift . state

instance Monad m => MonadError TextEditError (FoldMapLines r fold tags m) where
  throwError = FoldMapLines . lift . throwError
  catchError (FoldMapLines try) catch = FoldMapLines $ ContT $ \ next ->
    catchError (runContT try next) $ flip runContT next . unwrapFoldMapLines . catch

instance Monad m => MonadCont (FoldMapLines r fold tags m) where
  callCC f = FoldMapLines $ callCC $ unwrapFoldMapLines . f . (FoldMapLines .)

instance MonadTrans (FoldMapLines r fold tags) where
  lift = FoldMapLines . lift . lift . lift . lift

instance (Monad m, Semigroup a) => Semigroup (FoldMapLines r fold tags m a) where
  a <> b = (<>) <$> a <*> b

instance (Monad m, Monoid a) => Monoid (FoldMapLines r fold tags m a) where
  mappend a b = mappend <$> a <*> b
  mempty = return mempty

instance MonadIO m => MonadEditVec (MVec.IOVector (TextLine tags)) (FoldMapLines r fold tags m) where
  modVector          = foldMapLiftEditText . modVector
  modCount dir       = foldMapLiftEditText . modCount dir
  throwLimitErr      = foldMapLiftEditText . throwLimitErr

instance MonadEditText (FoldMapLines r fold) where { liftEditText = foldMapLiftEditText; }

instance MonadEditLine (FoldMapLines r fold) where
  liftEditLine = foldMapChars . liftEditLine

foldMapLiftEditText :: Monad m => EditText tags m a -> FoldMapLines r fold tags m a
foldMapLiftEditText = FoldMapLines . lift . lift . lift

-- | Convert a 'FoldMapLines' into an 'EditText' function. This function is analogous to the
-- 'runStateT' function. This function does not actually perform a fold or map operation, rather it
-- simply unwraps the 'EditText' monad that exists within the 'FoldMapLines' monad.
runFoldMapLinesStep
  :: Monad m
  => FoldMapLines r fold tags m a -> (a -> EditText tags m r) -> fold -> EditText tags m (r, fold)
runFoldMapLinesStep (FoldMapLines f) end =
  runStateT (runExceptT $ runContT f $ lift . lift . end) >=> \ case
    (Left err, _   ) -> throwError err
    (Right  a, fold) -> return (a, fold)

-- | Like 'runFoldMapLinesStep' but only returns the @fold@ result. This function is analogous to
-- the 'execStateT' function.
execFoldMapLinesStep
  :: Monad m
  => FoldMapLines fold fold tags m void -> fold -> EditText tags m fold
execFoldMapLinesStep (FoldMapLines f) = runStateT (runExceptT $ runContT f $ const get) >=> \ case
  (Left err, _   ) -> throwError err
  (Right  _, fold) -> return fold

-- | Like 'runFoldMapLinesStep' but ignores the @fold@ result. This function is analogous to the
-- 'evalStateT' function.
evalFoldMapLinesStep :: Monad m => FoldMapLines a fold tags m a -> fold -> EditText tags m a
evalFoldMapLinesStep f = fmap fst . runFoldMapLinesStep f return

-- | This function evaluates a 'MapLines' using 'evalFoldMapLines', performing a mapping operation
-- on only the line under the cursor. Note that this funcion must be evaluated within an 'EditText'
-- type of function. When using @do@ notation, it would look like this:
--
-- @
-- dotEndOfEveryLine :: EditText tags a
-- dotEndOfEveryLine = do
--     'gotoPosition' 0 0
--     'runMapLines' $ do
--         'gotoChar' 'Prelude.maxBound'
--         'insertChar' \'.\'
-- @
runMapLinesStep
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => MapLines tags m (TextLine tags) -> TextLine tags -> EditText tags m (TextLine tags)
runMapLinesStep f line = evalFoldMapLinesStep (f line) ()

----------------------------------------------------------------------------------------------------

-- | Functions of this type operate on a single 'TextLine', and can perform a fold over characters
-- in the line. This function type is polymorphic over 4 type variables
--
-- * @a@ is the monadic return type
--
-- * @tags@ is the @tags@ type of the 'TextBuffer' you are working on
--
-- * @fold@ is a value of your choosing that accumulates information, you can modify this value
--          using the 'Control.Monad.State.get' and 'Control.Monad.State.put' functions.
--
-- * @r@ the continuation return type (from a lifted continuation monad within 'FoldMapChars').
--       This simply means you can evaluate the 'Control.Monad.Cont.callCC' function to produce a
--       breaking function. Then within the folding/mapping function you may evaluate the breaking
--       function to halt and return from the fold or map operation immediately. The only
--       restriction on the type @r@ is that when you evaluate 'runFoldMapChars', @r@ must be the
--       same as the type @a@ of the function that was called with 'runFoldMapLines' function.
newtype FoldMapChars r fold tags m a
  = FoldMapChars
    { unwrapFoldMapChars :: ContT r (ExceptT TextEditError (StateT fold (EditLine tags m))) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => MonadState fold (FoldMapChars r fold tags m) where
  state = FoldMapChars . lift . state

instance Monad m => MonadError TextEditError (FoldMapChars r fold tags m) where
  throwError = FoldMapChars . lift . throwError
  catchError (FoldMapChars try) catch = FoldMapChars $ ContT $ \ next ->
    catchError (runContT try next) $ flip runContT next . unwrapFoldMapChars . catch

instance Monad m => MonadCont (FoldMapChars r fold tags m) where
  callCC f = FoldMapChars $ callCC $ unwrapFoldMapChars . f . (FoldMapChars .)

instance MonadEditLine (FoldMapChars r fold) where
  liftEditLine = FoldMapChars . lift . lift . lift

-- | Convert a 'FoldMapChars' into an 'FoldMapLine' function. This function is analogous to the
-- 'runStateT' function.
runFoldMapChars
  :: Monad m
  => FoldMapChars a fold tags m a -> fold -> EditLine tags m (a, fold)
runFoldMapChars (FoldMapChars f) = runStateT (runExceptT $ runContT f return) >=> \ case
  (Left err, _   ) -> throwError err
  (Right  a, fold) -> return (a, fold)

-- | Like 'runFoldMapChars' but only returns the @fold@ result. This function is analogous to the
-- 'execStateT' function.
execFoldMapChars
  :: Monad m
  => FoldMapChars fold fold tags m a -> fold -> EditLine tags m fold
execFoldMapChars (FoldMapChars f) = runStateT (runExceptT $ runContT f $ const get) >=> \ case
  (Left err, _   ) -> throwError err
  (Right  _, fold) -> return fold

-- | Like 'runFoldMapChars' but ignores the @fold@ result. This function is analogous to the
-- 'evalStateT' function.
evalFoldMapChars
  :: Monad m
  => FoldMapChars a fold tags m a -> fold -> EditLine tags m a
evalFoldMapChars = fmap (fmap fst) . runFoldMapChars

-- | Evaluate a 'FoldMapChars' function within a 'FoldMapLines' function, using the same @fold@
-- value from the 'FodlMapLines' state as the @fold@ value seen from within the 'FoldMapChars'
-- state. It is usually not necessary to invoke this function directly, the definition of
-- 'liftEditLine' for the 'FoldMapLines' function type is this function, so any function that
-- evaluates to an @editor@ where the @editor@ is a member of the 'MonadEditLine' typeclass will
-- automatically invoke this function based on the function type of the context in which it is used.
foldMapChars
  :: (MonadIO m
     , Show tags
     ) => FoldMapChars a fold tags m a -> FoldMapLines r fold tags m a
foldMapChars f = get >>= liftEditText . editLine . runFoldMapChars f >>= state . const

----------------------------------------------------------------------------------------------------

-- | A type synonym for a 'FoldMapLines' function in which the folded type is the unit @()@ value.
type MapChars r = FoldMapChars r ()

-- | Evaluate a 'MapChars' using 'evalFoldMapChars'.
runMapChars :: Monad m => MapChars a tags m a -> EditLine tags m a
runMapChars = flip evalFoldMapChars ()


----------------------------------------------------------------------------------------------------

-- | A type synonym for a 'FoldMapLines' function in which the return type is a list of 'TextLine'
-- values. You may rewrite the lines of 'TextBuffer' using the 'rewriteLines',
-- 'rewriteLinesInBuffer', and 'rewriteLinesInRange' functions. These functions iterate over all
-- lines in the 'TextBuffer', and allow you to choose whether to keep the line, remove it, or
-- replace it with one or more lines. The lines you return in the list are written back to the
-- buffer.
type RewriteLines r fold tags m = FoldMapLines r fold tags m [TextLine tags]

-- Not for export: this code shared by 'forLinesInRange' and 'forLines' but requires knowledge of
-- the 'TextBuffer' internals in order to use, so is not something end users should need to know
-- about.
rewriteLinesLoop
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => fold
  -> (FoldMapLinesHalt void fold tags m fold -> TextLine tags -> RewriteLines fold fold tags m)
  -> Int -> RelativeToCursor -> EditText tags m fold
rewriteLinesLoop fold f count dir = execFoldMapLinesStep (callCC $ loop count) fold where
  loop count halt =
    if count <= 0 then get else
    liftEditText (popElem dir) >>= f halt >>= mapM_ (pushElem (opposite dir)) >>
    loop (count - 1) halt
  -- TODO: Change the behavior and type of this function. The type should produce a monadic function
  -- that will read the current line from the buffer only when it is evaluated, and otherwise
  -- performs no read. The function should not pop the current line on every iteration, rather it
  -- should provide the option of reading the current line to the mapping function, then when the
  -- mapping function returns, the current line should be popped and discarded and the lines
  -- returned by the mapping function should be pushed.
  --
  -- It might be better to do away with the continuation monad transformer here, and instead provide
  -- some control symbols that can be returned by the maping function to control what the loop does
  -- between iterations.

-- | This function moves the cursor to the first @'Absolute' 'LineIndex'@ parameter given, then
-- evaluate a folding and mapping monadic function over a range of lines specified. If the first
-- 'LineIndex' parameter is greater than the second 'LineIndex' parameter, the fold map operation
-- evaluates in reverse line order.
--
-- If you do not want to lose the current cursor position, be sure to wrap the evaluation of this
-- function in a call to the 'saveCursorEval'.
--
-- The 'FoldMapLines' function you pass to this function will receive every 'TextLine' on and
-- between the two @'Absolute' 'TextIndex'@ parameters given, and can return zero or more updated
-- 'TextLine' values to replace the 'TextLine' received at each evaluation. Return an empty list to
-- delete the line, return the given 'TextLine' alone as a list of a single element to perform no
-- updating action to it.
--
-- Remember that the 'FoldMapLines' function type instantiates 'Control.Monad.Cont.State.MonadCont',
-- which means the 'FoldMapLines' function you pass to this function can elect to halt the fold map
-- operation by evaluating the stop function passed to it.
rewriteLinesInRange
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Absolute LineIndex -> Absolute LineIndex
  -> fold
  -> (FoldMapLinesHalt void fold tags m fold -> TextLine tags -> RewriteLines fold fold tags m)
  -> EditText tags m fold
rewriteLinesInRange absFrom@(Absolute (LineIndex from)) (Absolute (LineIndex to)) fold f = do
  gotoLine absFrom
  lineCount <- (+) <$> use linesAboveCursor <*> use linesBelowCursor
  let dist = to - from
  rewriteLinesLoop fold f (min lineCount . max 1 $ safeAbs dist) $ if dist < 0 then Before else After

-- | Conveniently calls 'forLinesInRange' with the first two parameters as @('Absolute' 1)@ and
-- @('Absolute' 'maxBound')@.
rewriteLinesInBuffer
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => fold
  -> (FoldMapLinesHalt void fold tags m fold -> TextLine tags -> RewriteLines fold fold tags m)
  -> EditText tags m fold
rewriteLinesInBuffer = rewriteLinesInRange (Absolute 1) maxBound

-- | Like 'rewriteLinesInRange', but this function takes a 'RelativeToCursor' value, iteration
-- begins at the cursor position where the 'bufferLineEditor' is set, and if the 'RelativeToCursor'
-- value is 'After' then iteration goes forward to the end of the buffer, whereas if the
-- 'RelativeToCursor' value is 'Before' then iteration goes backward to the start of the buffer.
rewriteLines
  :: ( MonadIO m
     , Show tags --DEBUG
     )
  => RelativeToCursor
  -> fold
  -> (FoldMapLinesHalt void fold tags m fold -> TextLine tags -> RewriteLines fold fold tags m)
  -> EditText tags m fold
rewriteLines rel fold f = do
  above <- use linesAboveCursor
  below <- use linesBelowCursor
  uncurry (rewriteLinesLoop fold f) $ case rel of
    Before -> (above, Before)
    After  -> (below, After)

----------------------------------------------------------------------------------------------------

-- | A type synonym for a 'FoldMapLines' function in which the folded type is the unit @()@
-- value. This is a special case of 'FoldMapLines', so use 'forLines', 'forLinesInRange', or
-- 'forLinesInBuffer' to evaluate a function of this type.
--
-- One reason you would use a function of this type, via the 'mapLines' or 'mapLinesInRange' or
-- 'mapLinesinBuffer' function, as opposed to a more general function like 'rewriteLines', is that
-- this function can make updates to the 'TextBuffer' without altering the cursor position, since
-- each iteration is guaranteed to output exactly one line for every one line of input it receives.
type MapLines tags m r = TextLine tags -> FoldMapLines r () tags m (TextLine tags)

-- | When evaluating 'mapLinesInRange', a 'MapLines' function is evaluated. The 'MapLines' function
-- type instantiates the 'Control.Monad.Cont.Class.MonadCont' type class, and the
-- 'Control.Monad.Class.callCC' function is evaluated before running the fold map operation,
-- producing a halting function. The halting function is of this data type.
--
-- Suppose you would like to map over lines 5 through 35 counting the lines as you go, but halt if
-- the line of text is @"stop\\n"@, you would evaluate 'forLinesInRange' like so:
--
-- @
-- stopSymbol <- 'Data.List.head' 'Control.Applicative.<$>' 'textLines' "stop\n"
-- 'mapLinesInRange' 5 35 $ \\ halt thisLine -> do
--     count <- 'Control.Monad.State.Class.get'
--     'Control.Monad.when' (thisLine == stopSymbol) halt
--     -- If the "halt" function was evaluated in the above "when" statement,
--     -- then the code below will not be evaluated.
--     'Control.Monad.State.Class.put' (count + 1)
--     return thisLine
-- @
type MapLinesHalt void tags m r = FoldMapLines r () tags m void

-- | Perform a 'MapLines' function on a range of lines.
mapLinesInRange
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => (MapLinesHalt void tags m () -> MapLines tags m ())
  -> Absolute LineIndex -> Absolute LineIndex
  -> EditText tags m ()
mapLinesInRange f from to = do
  from <- validateLineIndex from
  to   <- validateLineIndex to
  evalFoldMapLinesStep
    ( callCC $ \ halt -> do
        let contained buf       = forM_ [0 .. MVec.length buf - 1] $ \ i ->
              liftIO (MVec.read buf i) >>= f (halt ()) >>= liftIO . MVec.write buf i
        let straddle  buf1 buf2 = contained buf1 >> contained buf2
        withRegion (Absolute $ lineToIndex from) (Absolute $ lineToIndex to) straddle contained
    ) ()

-- | Perform a 'MapLines' function on all lines in the buffer.
mapLinesInBuffer
  :: Monad m
  => MapLines tags m a
  -> EditText tags m a
mapLinesInBuffer = error "TODO: mapLinesRange"

-- | Perform a 'MapLines' function relative to the cursor.
mapLines :: Monad m => MapLines tags m a -> RelativeToCursor -> EditText tags m a
mapLines = error "TODO: mapLines"

-- | Perform a 'MapLines' function on a range of lines. This function is identical to
-- 'mapLinesInRange, but takes the 'MapLines' continuation as the final parameter.
forLinesInRange
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Absolute LineIndex -> Absolute LineIndex
  -> (MapLinesHalt void tags m () -> MapLines tags m ())
  -> EditText tags m ()
forLinesInRange from to f = mapLinesInRange f from to

-- | This function is identical to 'mapLinesInBuffer'
forLinesInBuffer :: Monad m => MapLines tags m a -> EditText tags m a
forLinesInBuffer = mapLinesInBuffer

-- | Perform a 'MapLines' function relative to the cursor. This function is identical to
-- 'mapLinesInRange, but takes the 'MapLines' continuation as the final parameter.
forLines :: Monad m => RelativeToCursor -> MapLines tags m a -> EditText tags m a
forLines = flip mapLines
