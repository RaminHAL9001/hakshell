module Hakshell.TextEditor.FoldMap
  (
    -- * Editor Functions
    deleteCharsWrap,

    -- *** Rewriting Lines

    RewriteLines, rewriteLines, rewriteLinesInRange, rewriteLinesInBuffer,

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

    -- * Debugging

    debugPrintBuffer, debugPrintView, debugPrintCursor,

  ) where

import           Hakshell.TextEditor

import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.State

import qualified Data.Vector.Mutable         as MVec
import qualified Data.Vector.Generic.Mutable as GMVec

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

-- | Compute the 'TextCursorSpan' between two 'TextLocation's. The 'TextCursorSpan' is the number of
-- steps the text cursor must take to get from point @a@ to point @b@. For the most part, this is
-- equal to the number of characters that exist between point @a@ and point @b@, except when line
-- break characters consist of two characters (like the @'\\r\\n'@ combination) and, only in this
-- case, two characters are treated as one cursor step.
--
-- This function __DOES_NOT__ evlauate 'flushLineEditor', so you may get a results you are not
-- expecting if the text between the two given 'TextLocation's contains the line currently being
-- edited by the 'LineEditor'. If you want to make sure text in the 'LineEditor' is properly
-- accounted for in the result of this function, be sure to evaluate 'flushLineEditor' before
-- evaluating this function.
distanceBetween
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => TextLocation -> TextLocation -> EditText tags m TextCursorSpan
distanceBetween a0 b0 = do
  (a, lineA) <- validateLocation a0
  (b, lineB) <- validateLocation b0
  let forward = a <= b
  let ci = theLocationCharIndex
  let li = theLocationLineIndex
  let (liA, liB) = (li a, li b)
  let (sp, tcs) = (textLineCursorSpan, TextCursorSpan)
  let edge line a b = sp line - tcs (charToIndex $ ci a) + tcs (charToIndex $ ci b)
  let edgeSize = if forward then edge lineA a b else edge lineB b a
  let (nextA, prevB) = if forward then (liA + 1, liB - 1) else (liA - 1, liB + 1)
  (if forward then id else negate) <$>
    if liA == liB then return $ tcs $ charToIndex (ci b) - charToIndex (ci a)
    else if nextA == liB || prevB == liA then return edgeSize
    else foldLinesInRange nextA prevB edgeSize (\ _halt _i -> modify . (+) . sp)

-- | Compute the 'TextLocation' where the cursor would end up if you were to count a given number of
-- cursor steps (a value given by 'TextCursorSpan') from an initial 'TextLocation'. Also returns the
-- number of characters that were actually spanned, which may be less than the requested number.
--
-- This function __DOES_NOT__ evlauate 'flushLineEditor', so you may get a results you are not
-- expecting if the text between the two given 'TextLocation's contains the line currently being
-- edited by the 'LineEditor'. If you want to make sure text in the 'LineEditor' is properly
-- accounted for in the result of this function, be sure to evaluate 'flushLineEditor' before
-- evaluating this function.
spanDistance
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => TextLocation -> TextCursorSpan -> EditText tags m (TextLocation, TextCursorSpan)
spanDistance a dist = do
  let absDist = abs dist
  let forward = dist >= 0
  let constr size ln ch =
        (TextLocation{ theLocationLineIndex = ln, theLocationCharIndex = ch }, size)
  let distChar (TextCursorSpan i) = indexToChar i
  foldLinesInRange (theLocationLineIndex a)
    (if forward then maxBound else minBound) (a, 0) $ \ halt i line -> do
      let lineSize = textLineCursorSpan line
      oldCount <- gets snd
      let newCount = oldCount + lineSize
      let update = put $ constr newCount (if forward then i + 1 else i) 1
      if newCount == absDist then update >> halt ()
      else if newCount > absDist then do
        put $ constr absDist i $ distChar $
          if forward then absDist - oldCount else newCount - absDist
        halt ()
      else update

-- | Like 'moveByChar' but will wrap up to the previous line and continue moving on the
-- previous/next line if the value is large enough to move the cursor past the start\/end of the
-- line.
moveByCharWrap
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => TextCursorSpan -> EditText tags m TextLocation
moveByCharWrap dist = liftEditText $
  currentTextLocation >>= flip spanDistance dist >>= gotoPosition . fst

----------------------------------------------------------------------------------------------------

-- Not for export, because this function does not return a proper accounting of the characters it
-- deletes, it is expected that 'CharStats' accounting will be done by the functions which call this
-- one.
--
-- This function overwites all text before or after the cursor with a 'TextLine'. If 'Before', the
-- line breaking characters from the given 'TextLine' are ignored, if 'After', the line breaking
-- characters for the current line are overwritten with the line breaking characters of the given
-- 'TextLine'. Pass a function of two arguments which combines the tags from (1) the current line
-- editor and (2) the given 'TextLine'.
overwriteAtCursor
  :: MonadIO m
  => RelativeToCursor
  -> (tags -> tags -> tags)
  -> TextLine tags
  -> EditLine tags m ()
overwriteAtCursor rel concatTags line = case line of
  TextLineUndefined ->
    error $ "overwriteAtCursor "++show rel++": received undefined TextLine"
  TextLine
   { theTextLineString      = line
   , theTextLineBreakSymbol = lbrksz
   , theTextLineTags        = tagsB
   } -> do
    let myname = "overwriteAtCursor"
    modCount rel $ const 0 -- this "deletes" the chars Before/After the cursor
    count <- countDefined
    alloc <- getAllocSize
    let linelen  = UVec.length line
    let adjusted = linelen - case rel of { Before -> fromIntegral lbrksz; After -> 0; }
    let diffsize = adjusted + count - alloc
    when (diffsize > 0) $ growVector diffsize -- this resizes the line editor buffer
    buf   <- use lineEditBuffer -- new (resized) buffer
    alloc <- getAllocSize       -- new (resized) allocation value
    liftIO $ case rel of
      Before -> UVec.copy
        (asrtMSlice SafeOp myname asrtZero (asrtShow "adjusted" adjusted) ("lineEditBuffer", buf))
        (asrtSlice  SafeOp myname asrtZero (asrtShow "adjusted" adjusted) ("line", line))
      After  -> flip UVec.copy line $ asrtMSlice SafeOp myname
        (asrtShow "alloc-adjusted" $ alloc - adjusted)
        (asrtShow "adjusted" adjusted)
        ("buf", buf)
    cursorBreakSize .= lbrksz
    lineEditorIsClean .= False
    lineEditorTags    %= (`concatTags` tagsB)

-- Not for export: passing 'False' can leave the 'TextBuffer' in an inconsistent state.
-- 
-- This function operates on the 'LineEditor', deleting to the end of the line and then deleting the
-- line breaking characters at the end of the line. If 'True' is passed as an arguemtn and there are
-- more lines after the 'LineEditor' cursor position, pop and merge the next line into 'LineEditor'.
forwardDeleteLineBreak
  :: ( MonadIO m
     , Show tags --DEBUG
     )
  => Bool -> EditText tags m CharStats
forwardDeleteLineBreak mergeNext = do
  (after, lbrksz) <- use bufferLineEditor <&>
    theCharsAfterCursor &&& fromIntegral . theCursorBreakSize
  bufferLineEditor %=
    (charsAfterCursor .~ 0) .
    (cursorBreakSize .~ 0) .
    (lineEditorIsClean .~ False)
  when mergeNext $
    cursorAtEnd After >>=
    (`unless` (popElem After >>= editLine . overwriteAtCursor After const))
  return CharStats
    { cursorStepCount = TextCursorSpan $ after - lbrksz + 1
    , deltaCharCount  = negate $ Relative $ CharIndex after
    }

-- | This function deletes the given number of characters starting from the cursor and returns the
-- exact number of characters deleted, and if the number of characters to be deleted exceeds the
-- number of characters in the current line, characters are deleted from adjacent lines such that
-- the travel of deletion wraps to the end of the prior line or the beginning of the next line,
-- depending on the direction of travel. The direction of travel is determined by the sign of the
-- 'CharIndex' -- negative to delete moving toward the beginning, positive to delete moving toward
-- the end. The number of actual characters deleted is always positive.
--
-- __WARNING__: this function will __NOT__ call 'flushLineEditor', because we cannot assume that
-- committing the 'LineEditor' changes immediately is necessary or desireable, there may be more
-- edits to be done to the line after the deletion.
deleteCharsWrap
  :: ( MonadIO m
     , Show tags --DEBUG
     )
  => TextCursorSpan -> EditText tags m CharStats
deleteCharsWrap request =
  let direction = if request < 0 then Before else After in
  if request == 0 then return mempty
  -- First, if we can prove that the 'request' is not a 'minBound' or 'maxBound' value, we can use
  -- 'abs' rather than 'safeAbs' throughout the rest of this function.
  else if request >= maxBound || request <= minBound then deleteAllChars direction
  -- The first change we must make is to delete some of the the chararacters in the 'LineEditor',
  -- and see if that satisfies the request.
  else deleteChars request >>= \ st0 ->
  let satreq = cursorStepCount st0 in
  if abs satreq >= abs request then return st0
  -- Deleting characters from the line editor did not satisfy the request, but check if we are
  -- deleting forwards and if deleting just the line breaking character is enough to satisfy the
  -- request.
  else if direction == After && satreq + 1 >= request then
    -- Deleting the line break will satisfy the requset. We will do that and merge the next line
    -- into the 'LineEditor', if there even is a next line.
    (st0 <>) <$> forwardDeleteLineBreak True
  else do
    -- We have established that the number of requested character deletions will require removing
    -- all or part of adjacent lines. If we are deleting forward, delete the final line breaking
    -- character from the line editor, but DO NOT merge the next line into the 'LineEditor'.
    st0 <- if direction == After then (st0 <>) <$> forwardDeleteLineBreak False else pure st0
    -- Now let's use 'forLines' to delete each 'TextLine' until the request has been satsified.
    rewriteLines direction st0 $ \ halt line -> do
      st0 <- get
      -- If the previous iteration succeeded in deleting the requested number of steps, it should have
      -- preformed any final stateful updates, set the second element of the state to be equal to
      -- 'req', and then loop. So the first thing we do on each step of the loop is check if the
      -- number of deleted elements satisfies the request, and if so, we halt...
      if cursorStepCount st0 == request then halt st0 else do
        -- ...otherwise we delete more characters.
        let weight = textLineCursorSpan line
        let st = st0 <>
              CharStats
              { cursorStepCount = signum request * weight
              , deltaCharCount  = countToChar $ negate $ intSize line
              }
        -- If the request is still more than the entire current line, delete the line and loop...
        if abs (cursorStepCount st) <= abs request then put st else do
          -- ...otherwise, we need to delete part of the current line and merge the remainder into
          -- the 'LineEditor'.
          let (before, after) = flip splitLineAt line $ shiftAbsolute 1 $ countToChar $
                unwrapTextCursorSpan $ abs (cursorStepCount st0) - abs request + weight
          let (keep, delete, sign) = case direction of
                Before -> (before, after, negate)
                After  -> (after, before, id)
          editLine $ overwriteAtCursor direction const keep
          let st = st0 <>
                CharStats
                { cursorStepCount = sign $ textLineCursorSpan delete
                , deltaCharCount  = countToChar $ negate $ intSize delete
                } 
          put st
        return []

----------------------------------------------------------------------------------------------------

ralign :: Int -> String
ralign n = case safeAbs n of
  i | i < 10 -> "      " ++ show i
  i | i < 100 -> "     " ++ show i
  i | i < 1000 -> "    " ++ show i
  i | i < 10000 -> "   " ++ show i
  i | i < 100000 -> "  " ++ show i
  i | i < 1000000 -> " " ++ show i
  i                ->       show i

-- | Print debugger information about the structured data that forms the 'TextFrame' to standard
-- output. __WARNING:__ this print's every line of text in the view, so if your text view has
-- thousands of lines of text, there will be a lot of output.
debugPrintView :: MonadIO m => (tags -> String) -> (String -> IO ()) -> TextFrame tags -> m ()
debugPrintView showTags output view = do
  (_, errCount) <- forLinesInView view (1 :: Int, 0 :: Int) $ \ _halt line -> do
    lineNum <- state $ \ (lineNum, errCount) ->
      (lineNum, (lineNum + 1, errCount + if textLineIsUndefined line then 1 else 0))
    liftIO $ output $ ralign lineNum ++ ": " ++ showTextLine showTags line
  liftIO $ output ""
  when (errCount > 0) $ error $ "iterated over "++show errCount++" undefined lines"

-- | Print debugger information about the structured data that forms the 'TextBuffer' to standard
-- output. __WARNING:__ this print's every line of text in the buffer, so if your text buffer has
-- thousands of lines of text, there will be a lot of output.
debugPrintBuffer
  :: MonadIO m
  => (tags -> String) -> (String -> IO ()) -> EditText mvar tags m ()
debugPrintBuffer showTags output = do
  lineVec <- use bufferVector
  let len = MVec.length lineVec
  let printLines nullCount i = if i >= len then return () else do
        line <- liftIO $
          asrtMRead UnsafeOp "debugPrintBuffer" ("lineVec", lineVec) (asrtShow "i" i)
        let showLine = output $ ralign i ++ ": " ++ showTextLine showTags line
        if textLineIsUndefined line
          then do
            liftIO $ if nullCount < (1 :: Int) then showLine else
              when (nullCount < 4) $ putStrLn "...."
            (printLines $! nullCount + 1) $! i + 1
          else liftIO showLine >> (printLines 0 $! i + 1)
  printLines 0 0
  above   <- use linesAboveCursor
  below   <- use linesBelowCursor
  liftIO $ do
    output $ "   linesAboveCursor: " ++ show above
    output $ "   linesBelowCursor: " ++ show below
    output $ "    bufferLineCount: " ++ show (above + below)

-- | The 'bufferLineEditor', which is a 'LineEditor' is a separate data structure contained within
-- the 'TextBuffer', and it is often not necessary to know this information when debugging, so you
-- can print debugging information about the 'LineEditor' by evaluating this function whenever it is
-- necessary.
debugPrintCursor
  :: MonadIO m
  => (tags -> String) -> (String -> IO ()) -> EditText mvar tags m ()
debugPrintCursor showTags output = do
  cur <- use bufferLineEditor
  let charVec    = theLineEditBuffer cur
  let charVecLen = UMVec.length charVec
  let before     = cur ^. charsBeforeCursor
  let after      = cur ^. charsAfterCursor
  liftIO $ do
    str <- forM [0 .. charVecLen - 1] $
      asrtMRead UnsafeOp "debugPrintCursor" ("charVec", charVec) . (,) "i"
    output $ "     bufferLineEditor: " ++ show str
    output $ "       lineEditorTags: " ++ showTags (cur ^. lineEditorTags)
    output $ " __cursorVectorLength: " ++ show charVecLen
    output $ "    charsBeforeCursor: " ++ show before
    output $ "     charsAfterCursor: " ++ show after
    output $ "      cursorInputSize: " ++ show (before + after)
    output $ "  cursorLineBreakSize: " ++ show (theCursorBreakSize cur)
