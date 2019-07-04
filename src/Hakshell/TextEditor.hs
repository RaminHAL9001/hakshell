-- | This module integrates text processing facilities into Hakshell.
--
-- * Overview
--
-- This module provides editable text in the form of a 'TextBuffer', and read-only text in the form
-- of a 'TextView'.
--
-- Editing operations on a 'TextBuffer' are atomic and thread-safe, but do not alow for computations
-- on the buffer to be evaluated in parallel. Create a 'TextBuffer' with 'newTextBuffer', and then
-- evaluate 'EditText' functions using the 'runEditTextIO' function or 'runEditTextOnCopy'
-- function. 'EditText' combinators allow for moving the cursor around with functions such as
-- 'gotoPosition', and inserting text with functions such as 'insertChar' and 'insertString'.
--
-- Line breaks can be inserted using 'lineBreak'. Line breaks can be configured to follow the UNIX
-- tradition of using a single character '\n', or the DOS and CP/M tradition of using the
-- two-charater '\r\n' symbol. The default is UNIX style, with the 'defaultLineBreak', but the DOS
-- style can be set with 'lineBreakerNLCR'.
--
-- The 'TextBuffer' contains a local mutable line editor called a 'LineEditor'. Any single line from
-- within the buffer can be copied into the local 'LineEditor' using 'beginInsertMode'. The cursor
-- can be moved around using 'gotoChar' or 'moveByChar', characters can be inserted or deleted, then
-- the content of the 'LineEditor' can be copied back to the 'TextBuffer' using 'endInsertMode'.
--
-- Bulk edit operations can be performed using the 'forLinesInRangeM' function which evaluates a
-- continuation of type 'FoldMapLines' using a control flow pattern similar to a "for loop" in the
-- procedural programming style. The 'FoldMapLines' function lifts the ordinary 'EditText'
-- combinators type so edits can be evaluated on every line in the loop range. The 'FoldMapLines'
-- function type uses the 'Control.Monad.Cont.ContT' monad transformer internally so your bulk
-- editing algorithm can break out of the for loop at any time by simply evaluating the
-- 'FoldMapLinesHalt' function passed to your bulk editor function.
--
-- 'TextView's are a shallow, immutable snapshot of all, or a portion of, a 'TextBuffer'. The
-- internal structure of a 'TextView' is identical to that of the 'TextBuffer' so no additional
-- conversion or copying is necessary, making the creation of views extremely fast. Create a
-- 'TextView' using 'textViewOnLines' or 'textViewOnRange'. Once a 'TextView' is created other
-- threads may immediately resume performing updates on the 'TextBuffer'.
--
-- * Why a Hakshell needs a built-in editor
-- 
-- Any useful system shell needs some form of text editor. Some systems, like traditional UNIX
-- systems, prefer to keep the editor separate from the shell's command line interpreter. In Linux,
-- for example, you have Bash as the interpreter, and you can choose from a variety of editor
-- programs depending on your needs: Awk, Sed, Ed, Ex, Vi, Nano, Emacs, and so on. Sed and Awk are
-- for automatic text processing, where as Ed, Ex, Vi, Nano, and Emacs are for interactive text
-- editing.
--
-- You could argue that keeping an editor separate from the shell program provides for more
-- customizability and a good separation of concerns. But should these concerns really be separated?
-- I would argue that, no, a system shell should have integrated text editing functionality.
--
-- One of the most enduring innovations of UNIX is the UNIX philosophy: "everything is a file,"
-- allowing programs to be designed around the idiom of processing files. Likewise, the UNIX
-- userland tools were designed around automated text processing. The very notion of "pipes",
-- introduced by UNIX, declares a formal methodology for defining composable text filters. So the
-- distinction between a shell and an editor is really not so distinct. Binary data is of course can
-- also be piped, but as often as possible there are tools for representing the binary information
-- as text, and even re-constructing binary from it's textual representation.
--
-- Bash also has extensive functionality for manipulating text, for example, Globs and Regular
-- Expressions, which are also provided by the text editor programs. It is even theoretically
-- possible for both Bash and an editor like Vi to use the exact same glob pattern implementation by
-- linking these Bash or Vi against a glob pattern library. The only difference is the end users see
-- is that in Bash, these functions are invoked through the various Bash programming language
-- features (often embedded in Bash syntax, like when you run the command @ls *@), where as in an
-- editor like Vi or Emacs, these functions are invoked interactively, for example, when doing an
-- interactive text search/replace operation.
--
-- Emacs itself further eliminates the distinction between shell and text editor, so much so that it
-- is even arguable as to whether Emacs is a text editor at all. Some people jokingly refer to it as
-- an "operating system that lacks a useful text editor." A more accurate description of Emacs would
-- be a "Lisp interpreter that allows one to easily invent various interactive text editors for
-- various specific use cases." You could also call Emacs an IDE, or an app platform, both of which
-- have their text editing facilities provided for by the built-in Emacs text editor
-- functionality. Emacs also has very good shell integration: there are commands that execute shell
-- commands with buffered text as input, and capturing the output text of the process back into a
-- buffer.
--
-- Emacs can, to a great extent, perform most all of the same functions as Bash, while providing
-- countless additional interactive feautres. You can therefore think of Emacs a more advanced
-- system shell with integrated text editing features. It is even possible, although not commonly
-- done, to use Emacs in place of Bash as the login shell process, and anyone logging in to the
-- system would have no difficulty using any and all of the services provided to them by the server.
--
-- The "Hakshell.TextEditor" module is therefore designed to provide a solid, consistent foundation
-- for automated text processing APIs, upon which programmers can build text editors and text
-- processors, and hence a text editor API is an integral part of the "Hakshell" library.
module Hakshell.TextEditor
  ( -- * Text Editing API
    -- ** Create and Start Editing a 'TextBufferState'
    TextBuffer, newTextBuffer, copyTextBuffer,
    runEditTextIO, runEditTextOnCopy,
    runMapLines, runFoldMapLines, execFoldMapLines, evalFoldMapLines,
    -- ** Text Editing Combinators
    RelativeToCursor(..),
    insertString, insertChar, lineBreak,
    clearCurrentLine, resetCurrentLine,
    deleteChars, deleteCharsWrap, textCursorTags,
    currentTextLocation, currentLineNumber, currentColumnNumber,
    -- ** Manipulating Lines of Text
    beginInsertMode, endInsertMode,
    copyLineEditorText, replaceCurrentLine,
    pushLine, popLine,
    readLineIndex, writeLineIndex,
    forLines, forLinesInRange, forLinesInBuffer,
    -- * Text Views
    TextView, textView, textViewOnLines, textViewAppend, emptyTextView,
    newTextBufferFromView, textViewCharCount, textViewVector,
    FoldTextView, forLinesInView,
    -- * Cursor Positions
    Relative(..), Absolute(..), LineIndex(..), CharIndex(..), TextLocation(..),
    RelativeToAbsoluteCursor, -- <- does not export members
    relativeToAbsolute,
    relativeLine, relativeChar,
    lineIndex, charIndex, getCursor, gotoCursor, saveCursorEval,
    gotoPosition, gotoLine, gotoChar, moveCursor, moveByLine, moveByChar,
    -- * Function and Data Types
    -- ** Text Editing Typeclasses
    MonadEditText(..), MonadEditLine(..),
    -- ** Instances of Text Editing Type Classes
    EditText, copyLineEditorIO, newLineEditor, copyLineEditor,
    FoldMapLines, MapLines, EditLine, editLine,
    FoldMapLinesHalt,
    FoldMapChars, foldMapChars, runFoldMapChars, execFoldMapChars, evalFoldMapChars,
    MapChars, runMapChars,
    -- * Text Editor Data Structures
    -- ** Text Buffer
    TextBufferState, LineBreaker(..),
    bufferLineBreaker, lineBreaker, lineBreakPredicate, defaultLineBreak,
    bufferLineEditor, textLineString, bufferDefaultTags,
    lineBreakerNLCR,
    -- ** Line Editing
    TextLine, emptyTextLine, nullTextLine, sliceLineToEnd, textLineWeight, textLineIsUndefined,
    LineEditor, parentTextEditor, lineEditorCharCount, textLineTags,
    -- ** Errors
    TextEditError(..),
    -- * Debugging
    debugPrintBuffer, debugPrintView, debugPrintCursor,
    -- * Re-Exports
    -- ** "Hakshell.String"
    module Hakshell.String,
    -- ** "Control.Monad.State.Class"
    module Control.Monad.State.Class,
    -- ** "Control.Monad.Error.Class"
    module Control.Monad.Error.Class,
    -- ** "Control.Monad.Cont.Class"
    module Control.Monad.Cont.Class,
  ) where

import           Hakshell.String

import           Control.Arrow
import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.Cont.Class
import           Control.Monad.Except
import           Control.Monad.Error.Class
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.State.Class

import           Data.Semigroup
import qualified Data.Vector                 as Vec
import qualified Data.Vector.Generic         as GVec
import qualified Data.Vector.Mutable         as MVec
import qualified Data.Vector.Unboxed         as UVec
import qualified Data.Vector.Generic.Mutable as GMVec
import qualified Data.Vector.Unboxed.Mutable as UMVec
import           Data.Word

import Debug.Trace
--trace :: String -> a -> a
--trace = const id

--traceM :: Monad m => String -> m ()
--traceM = const $ return ()

----------------------------------------------------------------------------------------------------

-- Programmer notes:
--
-- A text editor buffer is a boxed mutable vector containing 'TextLine' objects. 'TextLine' objects
-- are ordinary data types which contain references to unboxed immutable vectors of characters. The
-- 'TextLine' is a sum type similar to 'Maybe' in that there is a null 'TextLineUndefined'
-- constructor used to instantiate an empty buffer. Buffers are pre-allocated to some exponent of 2
-- so lines being added does not always result in a re-allocation of the whole buffer. The buffer
-- usually contains 'TextLineUndefined' lines.
--
-- For efficient insertion, lines below the cursor are shifted toward the end of the vector, leaving
-- a gap of many conecutive 'TextLineUndefined' lines which can be over-written in O(1) time.
--
-- When a user positions the cursor, it is done with 1-based indexing values of type @('Absolute'
-- 'LineIndex')@ or @('Absolute' 'CharIndex')@, so the first line is line 1, not line zero. This
-- means care must be taken to translate 'Absolute' addresses to vector indicies by subtracting 1
-- from every line number parameter passed by a public API.
--
-- There are two integer values tracked in the text buffer: 'cursorLineIndex' and
-- 'linesBelowCursor'. The sum of these two numbers indicates how many lines there are in the
-- buffer. The cursor position, and the number of lines of text, is accounted for these two values.
--
-- It is important to remember, conceptually, that the cursor exists "in between" lines. When put
-- into "insert mode" (as in the Vi editor's way of doing things), a line is selected from just
-- below the cursor. So if the 'cursorLineIndex' value is 0, think of the cursor as being just above
-- the zero'th index in the buffer, and when a entering insert mode to begin editing a line, it
-- selects the line at index 0, which is @'Absolute' ('LineIndex' 1)@.
--
-- Another important thing to remember is that users express __inclusive__ line ranges. So when
-- selecting "from lines 2 to 3," the user expects both lines 2 and 3, so subtract 3 from 2 and add
-- 1 to get the total number of lines in the range. There are no semantics for a range of zero
-- lines, "lines 2 to 2" means select only line 2, a single line. Conceptually the end of the range
-- "from lines 2 to 3" means the cursor is between lines 3 and 4, so when translating this to vector
-- indicies, you want the cursor to end at just above line 4. If the buffer only contains 4 lines,
-- index 4 does not exist but it is still valid for computing the range of lines.

----------------------------------------------------------------------------------------------------

-- Any vector operations that have a safe (bounds-chekced) version and an unsafe version will be
-- switched to the unsafe version when this constant is set to True.
unsafeMode :: Bool
unsafeMode = False

-- -- I create let bindings for lenses often, so I often need the 'cloneLens' function. It is very
-- -- convenient to shorten the name to 'cl'.
-- cl :: ALens s t a b -> Lens s t a b
-- cl = cloneLens

-- Used by 'newTextBuffer' as a parameter to 'newTextBufferState'.
defaultInitBufferSize :: Int
defaultInitBufferSize = 512

----------------------------------------------------------------------------------------------------

-- | Used for indexing lines and characters relative to the cursor.
newtype Relative a = Relative a
  deriving (Eq, Ord, Show, Read, Enum, Num)

-- | Used for indexing absolute lines and characters (relative to the start of the document, which
-- is line 1).
newtype Absolute a = Absolute a
  deriving (Eq, Ord, Show, Read, Enum, Num)

-- | A number for indexing a line. This data type instantiates the 'Prelude.Num' typeclass so that
-- you can write an integer literal in your code and (if used in the correct context) the type
-- inference will automatically declare a 'LineIndex' without you needing to write @(LineIndex 1)@
-- constructor unless you really want to.
newtype LineIndex = LineIndex Int
  deriving (Eq, Ord, Show, Read, Enum, Num)

-- | A number for indexing a column, i.e. a character within a line. This data type instantiates
-- the 'Prelude.Num' typeclass so that you can write an integer literal in your code and (if used in
-- the correct context) the type inference will automatically declare a 'CharIndex' without you
-- needing to write @(LineIndex 1)@ constructor unless you really want to.
newtype CharIndex = CharIndex Int
  deriving (Eq, Ord, Show, Read, Enum, Num)

instance Bounded (Absolute CharIndex) where
  minBound = Absolute $ CharIndex 1
  maxBound = Absolute $ CharIndex maxBound

instance Bounded (Absolute LineIndex) where
  minBound = Absolute $ LineIndex 1
  maxBound = Absolute $ LineIndex maxBound

----------------------------------------------------------------------------------------------------

-- | This is a type of functions that can modify the textual content stored in a 'TextBufferState'.
newtype EditText tags m a
  = EditText
    { unwrapEditText ::
        ReaderT (TextBuffer tags) (ExceptT TextEditError (StateT (TextBufferState tags) m)) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => MonadState (TextBufferState tags) (EditText tags m) where
  state = EditText . lift . state

instance Monad m => MonadError TextEditError (EditText tags m) where
  throwError = EditText . throwError
  catchError (EditText try) catch = EditText $ catchError try $ unwrapEditText . catch

instance MonadTrans (EditText tags) where
  lift = EditText . lift . lift . lift

-- | Evaluate an 'EditText' function on the given 'TextBufferState'. The given 'EditText' function
-- is evaluates all updates on the given 'TextBuffer' atomically and in-place (i.e. without copying
-- anything unless instructed to do so). This is the most efficient way to evaluate an 'EditText'
-- function, but is more restrictive in that it can only be evaluated when the 'EditText' data
-- type's @m@ parameter is set to the @IO@ type, meaning you cannot use this function if the
-- 'EditText's @m@ parameter is something other than @IO@, like for example a 'ReaderT' type.
runEditTextIO :: EditText tags IO a -> TextBuffer tags -> IO (Either TextEditError a)
runEditTextIO (EditText f) this@(TextBuffer mvar) = modifyMVar mvar $
  fmap (\ (a,b) -> (b,a)) . runStateT (runExceptT $ runReaderT f this)

-- | Evaluate an 'EditText' function on the given 'TextBufferState', but unlike 'runEditTextIO', a
-- copy of the entire text buffer is first created, and all updates are performed on the copy
-- atomically. This function is less restrictive than 'runEditTextIO' because it will work for
-- 'EditText' functions where the @m@ parameter is not just @IO@ but any member of the 'Monad'
-- typeclass, however the trade-off is that a copy of the text buffer must be made.
runEditTextOnCopy
  :: MonadIO m
  => EditText tags m a -> TextBuffer tags -> m (Either TextEditError a, TextBuffer tags)
runEditTextOnCopy (EditText f) = liftIO . copyTextBuffer >=> \ copy@(TextBuffer mvar) -> do
  (result, st) <- liftIO (readMVar mvar) >>= runStateT (runExceptT $ runReaderT f copy)
  liftIO $ void $ swapMVar mvar st
  return (result, copy)

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

-- | Convert a 'FoldMapLines' into an 'EditText' function. This function is analogous to the
-- 'runStateT' function. This function does not actually perform a fold or map operation, rather it
-- simply unwraps the 'EditText' monad that exists within the 'FoldMapLines' monad.
runFoldMapLines
  :: Monad m
  => FoldMapLines a fold tags m a -> fold -> EditText tags m (a, fold)
runFoldMapLines (FoldMapLines f) = runStateT (runExceptT $ runContT f return) >=> \ case
  (Left err, _   ) -> throwError err
  (Right  a, fold) -> return (a, fold)

-- | Like 'runFoldMapLines' but only returns the @fold@ result. This function is analogous to the
-- 'execStateT' function.
execFoldMapLines
  :: Monad m
  => FoldMapLines fold fold tags m void -> fold -> EditText tags m fold
execFoldMapLines (FoldMapLines f) = runStateT (runExceptT $ runContT f $ const get) >=> \ case
  (Left err, _   ) -> throwError err
  (Right  _, fold) -> return fold

-- | Like 'runFoldMapLines' but ignores the @fold@ result. This function is analogous to the
-- 'evalStateT' function.
evalFoldMapLines :: Monad m => FoldMapLines a fold tags m a -> fold -> EditText tags m a
evalFoldMapLines = fmap (fmap fst) . runFoldMapLines

----------------------------------------------------------------------------------------------------

-- | A type synonym for a 'FoldMapLines' function in which the folded type is the unit @()@ value.
type MapLines r tags m a = FoldMapLines r () tags m a

-- | This function evaluates a 'MapLines' using 'evalFoldMapLines' without actually performing a
-- mapping operation, rather this function simply unwraps the 'MapLines' monad, converting it to an
-- ordinary 'EditText' monad. Note that this funcion must be evaluated within an 'EditText' type of
-- function. When using @do@ notation, it would look like this:
--
-- @
-- dotEndOfEveryLine :: EditText tags a
-- dotEndOfEveryLine = do
--     'gotoPosition' 0 0
--     'runMapLines' $ do
--         'gotoChar' 'Prelude.maxBound'
--         'insertChar' \'.\'
-- @
runMapLines :: Monad m => MapLines a tags m a -> EditText tags m a
runMapLines = flip evalFoldMapLines ()

----------------------------------------------------------------------------------------------------

-- | Functions of this type operate on a single 'TextLine', it is useful for updating the
-- 'bufferLineEditor'.
newtype EditLine tags m a
  = EditLine
    { unwrapEditLine :: ExceptT TextEditError (StateT (LineEditor tags) (EditText tags m)) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => MonadState (LineEditor tags) (EditLine tags m) where
  state = EditLine . lift . state

instance Monad m =>  MonadError TextEditError (EditLine tags m) where
  throwError = EditLine . throwError
  catchError (EditLine try) catch = EditLine $ catchError try $ unwrapEditLine . catch

instance MonadTrans (EditLine tags) where
  lift = EditLine . lift . lift . lift

-- | Perform an edit on the line under the cursor. It is usually not necessary to invoke this
-- function directly, the definition of 'liftEditLine' for the 'TextEdit' function type is this
-- function, so any function that evaluates to an @editor@ where the @editor@ is a member of the
-- 'MonadEditLine' typeclass will automatically invoke this function based on the function type of
-- the context in which it is used.
editLine
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => EditLine tags m a -> EditText tags m a
editLine (EditLine f) = use bufferLineEditor >>= runStateT (runExceptT f) >>= \ case
  (Left err, _   ) -> throwError err
  (Right  a, line) -> bufferLineEditor .= line >> return a

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
--       restriction is that the type @r@ always be the same as the type @a@ of the function that
--       was called with 'runFoldMapLines' function.
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

-- | This is a reference to the stateful data of your buffer of text. You edit this text by
-- evaluating any text editing combinators that evaluate to an 'EditText' function type. Declare a
-- new 'TextBuffer' using the 'newTextBuffer' function, then pass this 'TextBuffer' to the
-- 'runEditText' function along with some combinator functions of type @('EditText' tags)@ or type
-- @'MonadEditText' editor => (editor tags)@.
--
-- @
-- main :: IO ()
-- main = do
--     buf <- 'newTextBuffer' ()
--     'runEditText' $ do
--         'insertString' "Hello, world!\\nThis is a text document.\\n"
--         'gotoPosition' 1 0 'Control.Monad.>>' 'insertChar' \'"\'
--         'moveByChar' 'Prelude.maxBound' 'Control.Monad.>>' 'insertChar' '"'
--     'Control.Monad.return' ()
-- @
--
-- You may also notice some of the combinator functions in this module are of the type @(editor tags
-- a)@ (all lower-case, all type variables) such that the @editor@ type variable is an instance of
-- the 'MonadEditText' type class and @(editor tags)@ type variable is an instance of the 'Monad'
-- typeclass. Since the 'EditText' function does instantiate both the 'MonadEditText' and 'Monad'
-- typeclasss, this means it is possible to use any function which is defined be of type @(editor
-- tags a)@ type as if it were an 'EditText' function.
--
-- Just remember that you can't use an @(editor tag a@) type as though @editor@ were two different
-- function types, for example when using an 'EditText' and an 'FoldMapLines' function in the same
-- @do@ block, this will fail to pass the type-checker.
newtype TextBuffer tags = TextBuffer (MVar (TextBufferState tags))
  deriving Eq

-- | This data type stores a buffer of editable text. This is stateful information for the
-- 'TextBuffer' data type. The constructor for this data type is not exposed because it is
-- automatically constructed by the 'newTextBuffer' function, and it contains what object oriented
-- programmers would call "private" variables. However some of the lenses for this data type, namely
-- 'bufferDefaultTags', 'bufferLineBreaker', and 'bufferLineEditor', do allow you to modify the
-- variable fields of this data type.
--
-- The 'EditText' monad instantiates the 'MonadState' typeclass such that the stateful data is this
-- data type, which means you can use the 'Control.Lens.use', 'Control.Lens.assign', functions and
-- the similar @('Control.Lens..=')@, @('Control.Lens.%=') operators in the "Control.Lens" module to
-- update these values from within a @do@ block of code when programming a text editing combinator.
data TextBufferState tags
  = TextBufferState
    { theBufferDefaultLine :: !(TextLine tags)
      -- ^ The tag value to use when new 'TextLine's are automatically constructed after a line
      -- break character is inserted.
    , theBufferLineBreaker :: LineBreaker
      -- ^ The function used to break strings into lines. This function is called every time a
      -- string is transferred from 'theBufferLineEditor' to to 'theLinesAbove' or 'theLinesBelow'.
    , theBufferVector      :: !(MVec.IOVector (TextLine tags))
      -- ^ A mutable vector containing each line of editable text.
    , theLinesAboveCursor  :: !Int
      -- ^ The number of lines above the cursor
    , theLinesBelowCursor  :: !Int
      -- ^ The number of line below the cursor
    , theBufferLineEditor  :: !(LineEditor tags)
      -- ^ A data structure for editing individual characters in a line of text.
    , theBufferInsertMode  :: !Bool
      -- ^ Whether or not the buffer is in insert mode, which means a 'TextLine' has been copied
      -- into 'theBufferLineEditor'.
    }

data TextEditError
  = TextEditError       !StrictBytes
  | EndOfLineBuffer     !RelativeToCursor
  | EndOfCharBuffer     !RelativeToCursor
  | LineIndexOutOfRange !(Absolute LineIndex)
  | LineCountOutOfRange !(Relative LineIndex)
  | CharIndexOutOfRange !(Absolute CharIndex)
  | CharCountOutOfRange !(Relative CharIndex)
  deriving (Eq, Ord, Show)

-- | A pair of functions used to break strings into lines. This function is called every time a
-- string is transferred from 'theBufferLineEditor' to to 'theLinesAbove' or 'theLinesBelow' to ensure
-- all strings entered into a buffer have no more than one line terminating character sequence at
-- the end of them.
--
-- If you choose to use your own 'LineBreaker' function, be sure that the function obeys this law:
--
-- @
-- 'Prelude.concat' ('lineBreakerNLCR' str) == str
-- @
data LineBreaker
  = LineBreaker
    { theLineBreakPredicate :: Char -> Bool
      -- ^ This function is called by 'insertChar' to determine if the 'bufferLineEditor' should be
      -- terminated.
    , theLineBreaker :: String -> [(String, String)]
      -- ^ This function scans through a string finding character sequences that delimit the end of
      -- a line of text. For each returned tuple, the first element of the tuple should be a string
      -- without line breaks, the second element should contain a string with only line breaks, or
      -- an empty string if the string was not terminated with a line break.
    , theDefaultLineBreak :: !CharVector
      -- ^ This defines the default line break to be used by the line breaking function.
    }

----------------------------------------------------------------------------------------------------

-- | 'EditText' functions operate on units of text, and each unit of text is the "line," which is
-- usually a @'\n'@ character terminated line of text in a text file, although it could represent
-- other things, like files in a directory, or rows in a table of a database. Since a 'ExitText'
-- functions perform similar operations no matter what the line type actually is, this data type is
-- actually a 'Functor' which can store arbitrarily typed data (symbolized by the type variable @a@)
-- along with the actual text payload.
--
-- This arbitrary data can be useful for storing the state of a parser used by a syntax hilighter,
-- it can be used to store searchable tags, can contain sub-documents that may be expanded to text
-- at a later time, or all of the above.
--
-- Note that when a user user is editing text interactively, they are not operating on a
-- 'TextLine', but on a 'Prelude.String' (list of 'Char's) stored in the 'TextBufferState' state.
data TextLine tags
  = TextLineUndefined
  | TextLine
    { theTextLineString    :: !CharVector
    , theTextLineTags      :: !tags
    , theTextLineBreakSize :: !Word16
    }
  deriving Functor

instance IntSized (TextLine tags) where
  intSize = \ case
    TextLineUndefined               -> 0
    TextLine{theTextLineString=str} -> UVec.length str

instance Unpackable (TextLine tags) where
  unpack = \ case
    TextLineUndefined               -> ""
    TextLine{theTextLineString=str} -> UVec.toList str

instance Show tags => Show (TextLine tags) where
  show = \ case
    TextLineUndefined -> "(null)"
    TextLine{theTextLineString=vec,theTextLineTags=tags,theTextLineBreakSize=lbrksz} ->
      '(' : show (unpack vec) ++ ' ' : show lbrksz ++ ' ' : show tags ++ ")"

-- | The empty 'TextLine' value.
emptyTextLine :: tags -> TextLine tags
emptyTextLine tags = TextLine
  { theTextLineString    = UVec.empty
  , theTextLineTags      = tags
  , theTextLineBreakSize = 0
  }

-- | Evaluates to 'True' if the 'TextLine' is empty. Undefined lines also evaluate to 'True'. (Use
-- 'textLineIsUndefined' to test if the line is undefined.) If the line is empty and not undefined,
-- the given predicate is evaluated on the 'textLineTags' value, and this predicate's result
-- determines whether the line is null.
nullTextLine :: (tags -> Bool) -> TextLine tags -> Bool
nullTextLine nullTags = \ case
  TextLineUndefined -> True
  line              ->
    if UVec.null (theTextLineString line) then nullTags (theTextLineTags line) else False

-- | Cut a line at some index, keeping the characters 'Before' or 'After' the index.
sliceLineToEnd :: RelativeToCursor -> Absolute CharIndex -> TextLine tags -> TextLine tags
sliceLineToEnd rel (Absolute (CharIndex n)) = \ case
  TextLineUndefined -> TextLineUndefined
  line              -> line & textLineString %~ \ vec ->
    let len = UVec.length vec in case rel of
      Before -> UVec.slice  0 (len - n) vec
      After  -> UVec.slice (len - n) n  vec

-- | Evaluates toe True if the 'TextLine' is undefined. An undefined 'TextLine' is different from an
-- empty string, it is similar to the 'Prelude.Nothing' constructor.
textLineIsUndefined :: TextLine tags -> Bool
textLineIsUndefined = \ case { TextLineUndefined -> True; _ -> False; }

-- | The weight of a 'TextLine' is essentially the number of unit characters the 'TextLine'
-- contains, which most of the time is exactly equal to the vector length of 'theTextLineString',
-- but not always. In some cases, a 'TextLine' has multiple line break characters, especially on the
-- Windows operating system in which a line break contains two characters: @"\\n\\r"@. However when
-- using a text editor on such an operating system, pressing the backspace key on the keyboard when
-- at the start of the line will wrap the deletion operation to the previous line, deleting all
-- newline characters in a single key press. Thus the two characters @"\\n\\r"@ have a weight of 1
-- key press.
--
-- This text editor engine accommodates arbitrary line break character sequences by providing this
-- 'textLineWeight' metric, which always treats all line breaking characters as a single unit. If
-- there are no line breaking characters, or if there is only one line breaking character, the
-- 'textLineWeight' is identical to the value given by 'intSize'.
textLineWeight :: TextLine tags -> Int
textLineWeight line = intSize line + fromIntegral (min 1 $ max 0 $ theTextLineBreakSize line) 

----------------------------------------------------------------------------------------------------

-- | The current line that is being edited.
data LineEditor tags
  = LineEditor
    { theLineEditBuffer    :: !(UMVec.IOVector Char)
    , parentTextEditor     :: (TextBuffer tags)
      -- ^ A line editor can be removed from it's parent 'TextEditor'. A 'LineEditor' is defined
      -- such that it can only directly edit text in it's parent 'TextEditor'. A 'LineEditor' can
      -- indirectly edit text in any other parent, but only if a complete copy of the content of the
      -- line editor is made and passed it to some other 'TextEditor'.
    , theCharsBeforeCursor :: !Int
    , theCharsAfterCursor  :: !Int
    , theCursorBreakSize   :: !Word16
    , theLineEditorTags    :: tags
    }

-- Not for export: this buffer is formatted such that characters before the cursror are near index
-- zero, while characters after the cursor are near the final index.
lineEditBuffer :: Lens' (LineEditor tags) (UMVec.IOVector Char)
lineEditBuffer = lens theLineEditBuffer $ \ a b -> a{ theLineEditBuffer = b }

-- Not for export: this gets updated when inserting or deleting characters before the cursor.
charsBeforeCursor :: Lens' (LineEditor tags) Int
charsBeforeCursor = lens theCharsBeforeCursor $ \ a b -> a{ theCharsBeforeCursor = b }

-- Not for export: this gets updated when inserting or deleting characters after the cursor.
charsAfterCursor :: Lens' (LineEditor tags) Int
charsAfterCursor = lens theCharsAfterCursor $ \ a b -> a{ theCharsAfterCursor = b}

-- Not for export: controls line break information within the cursor. If this value is zero, it
-- indicates that no line breaking characters have been entered into the cursor yet. If this value
-- is non-zero, it indicates that the line breaking characters do exist after the cursor at some
-- point, and if additional line breaks are inserted the characters after the cursor need to be
-- split off into a new 'TextLine'.
cursorBreakSize :: Lens' (LineEditor tags) Word16
cursorBreakSize = lens theCursorBreakSize $ \ a b -> a{ theCursorBreakSize = b }

-- | A 'Control.Lens.Lens' to get or set tags for the line currently under the cursor. To use or
-- modify the tags value of the line under the cursor, evaluate one of the functions 'use',
-- 'modifying', @('Control.Lens..=')@, or @('Control.Lens.%=')@ within an 'EditText' function, or
-- any function which instantiates 'MonadEditText'.
textCursorTags :: Lens' (LineEditor tags) tags
textCursorTags = lens theLineEditorTags $ \ a b -> a{ theLineEditorTags = b }

-- | Use this to initialize a new empty 'TextBufferState'. The default 'bufferLineBreaker' is set to
-- 'lineBreakerNLCR'. A 'TextBufferState' always contains one empty line, but a line must have a @tags@
-- tag, so it is necessary to pass an initializing tag value of type @tags@ -- if you need nothing
-- but plain text editing, @tags@ can be unit @()@.
newTextBuffer :: tags -> IO (TextBuffer tags)
newTextBuffer tags = do
  mvar <- newEmptyMVar
  let this = TextBuffer mvar
  newTextBufferState this defaultInitBufferSize tags >>= putMVar mvar
  return this

-- | Create a deep-copy of a 'TextBuffer'. Everything is copied perfectly, including the cursor
-- position, and the content and state of the cursor.
copyTextBuffer :: TextBuffer tags -> IO (TextBuffer tags)
copyTextBuffer (TextBuffer mvar) = withMVar mvar $ \ old -> do
  newBuf <- copyVec (theBufferVector old) (theLinesAboveCursor old) (theLinesBelowCursor old)
  newCur <- copyLineEditorIO $ theBufferLineEditor old
  fmap TextBuffer $ newMVar $ old{ theBufferLineEditor = newCur, theBufferVector = newBuf }

newTextBufferState :: TextBuffer tags -> Int -> tags -> IO (TextBufferState tags)
newTextBufferState this size tags = do
  cur <- newLineEditorIO this tags
  buf <- MVec.replicate size TextLineUndefined
  let emptyLine = TextLine
        { theTextLineTags      = tags
        , theTextLineString    = mempty
        , theTextLineBreakSize = 0
        }
  MVec.write buf 0 emptyLine
  return TextBufferState
    { theBufferDefaultLine = emptyLine
    , theBufferLineBreaker = lineBreakerNLCR
    , theBufferVector      = buf
    , theLinesAboveCursor  = 1
    , theLinesBelowCursor  = 0
    , theBufferLineEditor  = cur
    , theBufferInsertMode  = False
    }

-- Use this to initialize a new empty 'LineEditor'. This is usually only handy if you want to
-- define and test your own 'EditLine' functions and need to evaluate 'editLine' by hand rather than
-- allowing the 'TextEdit' APIs automatically manage line editing. A 'TextBufferState' always
-- contains one empty line, but a line must have a @tags@ tag, so it is necessary to pass an
-- initializing tag value of type @tags@.
--
-- See also the 'newLineEditor' function which calls this function within a 'MonadEditLine' context.
newLineEditorIO :: TextBuffer tags -> tags -> IO (LineEditor tags)
newLineEditorIO parent tag = do
  buf <- UMVec.new 1024
  return LineEditor
    { theLineEditBuffer    = buf
    , parentTextEditor     = parent
    , theCharsBeforeCursor = 0
    , theCharsAfterCursor  = 0
    , theCursorBreakSize   = 0
    , theLineEditorTags    = tag
    }

-- | Use this to create a deep-copy of a 'LineEditor'. The cursor position within the 'LineEditor'
-- is also copied.
--
-- See also the 'copyLineEditor' function which calls this function within an 'MonadEditText'
-- context.
copyLineEditorIO :: LineEditor tags -> IO (LineEditor tags)
copyLineEditorIO cur = do
  buf <- copyVec (theLineEditBuffer cur) (theCharsBeforeCursor cur) (theCharsAfterCursor cur)
  return cur{ theLineEditBuffer = buf }

-- | Determine how many characters have been stored into this buffer.
lineEditorCharCount :: LineEditor tags -> Int
lineEditorCharCount cur = theCharsBeforeCursor cur + theCharsAfterCursor cur

-- Create a new 'LineEditor' from a 'TextLine'. The 'LineEditor' can be updated with an 'EditLine'
-- function. Note that this function works in any monadic function type @m@ which instantiates
-- 'Control.Monad.IO.Class.MonadIO', so this will work in the @IO@ monad, the 'EditText' monad, the
-- 'EditLine' monad, and in other contexts as well.
newCursorFromLine :: MonadIO m => TextBuffer tags -> Int -> TextLine tags -> m (LineEditor tags)
newCursorFromLine parent cur line = liftIO $ do
  -- This function is safe to export because we assume a 'TextLine' never contains more than one
  -- line terminator, and always only at the end of the buffer. The 'TextLine' constructor is not
  -- exported.
  let str = line ^. textLineString
  let len = intSize str
  let breakSize = theTextLineBreakSize line
  cur <- pure $ min (max 0 $ len - fromIntegral breakSize) $ max 0 cur
  let (before, after) = splitAt cur $ unpack str
  let (lenBefore, lenAfter) = (length before, length after)
  let strlen = lenBefore + lenAfter
  let len = head $ takeWhile (< strlen) $ iterate (* 2) 1024
  buf <- UMVec.new len
  forM_ (zip [0 .. lenBefore - 1] before) $ uncurry $ UMVec.write buf
  forM_ (zip [len - lenAfter .. len - 1] after) $ uncurry $ UMVec.write buf
  return LineEditor
    { theLineEditBuffer    = buf
    , parentTextEditor     = parent
    , theCharsBeforeCursor = lenBefore
    , theCharsAfterCursor  = lenAfter
    , theCursorBreakSize   = breakSize
    , theLineEditorTags    = theTextLineTags line
    }

-- | This is the default line break function. It will split the line on the character sequence
-- @"\n"@, or @"\r"@, or @"\n\r"@, or @"\r\n"@. The line terminators must be included at the end of
-- each broken string, so that the rule that the law
-- @'Prelude.concat' ('theLineBreaker' 'lineBreakerNLCR' str) == str@ is obeyed.
lineBreakerNLCR :: LineBreaker
lineBreakerNLCR = LineBreaker
  { theLineBreakPredicate = nlcr
  , theLineBreaker = lines
  , theDefaultLineBreak = UVec.fromList "\n"
  } where
    nlcr c = c == '\n' || c == '\r'
    lines  = break nlcr >>> \ case
      (""  , "") -> []
      (line, "") -> [(line, "")]
      (line, '\n':'\r':more) -> (line, "\n\r") : lines more
      (line, '\r':'\n':more) -> (line, "\r\n") : lines more
      (line, c:more)         -> (line, [c])    : lines more

-- Not for export: this is only used to set empty lines in the buffer.
bufferDefaultLine :: Lens' (TextBufferState tags) (TextLine tags)
bufferDefaultLine = lens theBufferDefaultLine $ \ a b -> a{ theBufferDefaultLine = b }

-- | Entering a line-breaking character (e.g. @'\n'@) into a 'TextBufferState' using 'insertChar' or
-- 'insertString' results in several 'TextLine's being generated automatically. Whenever a
-- 'TextLine' is constructed, there needs to be a default tag value that is assigned to it. This
-- lens allows you to observe or set the default tag value.
bufferDefaultTags :: Lens' (TextBufferState tags) tags
bufferDefaultTags = bufferDefaultLine . textLineTags

-- | The function used to break strings into lines. This function is called every time a string is
-- transferred from 'theBufferLineEditor' to 'theLinesAboveCursor' or 'theLinesBelow'. Note that setting
-- this function doesn't restructure the buffer, the old line breaks will still exist as they were
-- before until the entire buffer is refreshed.
--
-- If you choose to use your own 'LineBreaker' function, be sure that the function obeys this law:
--
-- @
-- 'Prelude.concat' ('lineBreakerNLCR' str) == str
-- @
bufferLineBreaker :: Lens' (TextBufferState tags) LineBreaker
bufferLineBreaker = lens theBufferLineBreaker $ \ a b -> a{ theBufferLineBreaker = b }

-- | This function is called by 'insertChar' to determine if the 'bufferLineEditor' should be
-- terminated.
lineBreakPredicate :: Lens' LineBreaker (Char -> Bool)
lineBreakPredicate = lens theLineBreakPredicate $ \ a b -> a{ theLineBreakPredicate = b }

-- | This function scans through a string finding character sequences that delimit the end of a line
-- of text.
lineBreaker :: Lens' LineBreaker (String -> [(String, String)])
lineBreaker = lens theLineBreaker $ \ a b -> a{ theLineBreaker = b }

-- | This defines the default line break to be used by the line breaking function.
defaultLineBreak :: Lens' LineBreaker CharVector
defaultLineBreak = lens theDefaultLineBreak $ \ a b -> a{ theDefaultLineBreak = b }

-- Not for export: requires correct accounting of line numbers to avoid segment faults.
linesAboveCursor :: Lens' (TextBufferState tags) Int
linesAboveCursor = lens theLinesAboveCursor $ \ a b -> a{ theLinesAboveCursor = b }

-- Not for export: requires correct accounting of line numbers to avoid segment faults.
linesBelowCursor :: Lens' (TextBufferState tags) Int
linesBelowCursor = lens theLinesBelowCursor $ \ a b -> a{ theLinesBelowCursor = b }

-- Not for export: the vector containing all the lines of text in this buffer.
bufferVector :: Lens' (TextBufferState tags) (MVec.IOVector (TextLine tags))
bufferVector = lens theBufferVector $ \ a b -> a{ theBufferVector = b }

-- Not for export: needs to be set by higher-level combinators to prevent a line in the line editor
-- cursor from being accidentally copied to more than one index in the line buffer.
bufferInsertMode :: Lens' (TextBufferState tags) Bool
bufferInsertMode = lens theBufferInsertMode $ \ a b -> a{ theBufferInsertMode = b }

-- | The current line of text being edited under the cursor.
bufferLineEditor :: Lens' (TextBufferState tags) (LineEditor tags)
bufferLineEditor = lens theBufferLineEditor $ \ a b -> a{ theBufferLineEditor = b }

-- | The null-terminated, UTF-8 encoded string of bytes stored in this line of text.
textLineString :: Lens' (TextLine tags) CharVector
textLineString = lens theTextLineString $ \ a b -> a{ theTextLineString = b }

-- | Arbitrary information stored with this text. Typcial use cases for this field may include
-- syntax coloring rules, structured data parsed from the 'textLineString', text search indicies, a
-- diff of changes made, or all of the above.
textLineTags :: Lens' (TextLine tags) tags
textLineTags = lens theTextLineTags $ \ a b -> a{ theTextLineTags = b }

----------------------------------------------------------------------------------------------------

-- Line indexing arithmetic
--
-- I think of these functions as being similar to named environment variables in that you can use
-- these names to have meaningful symbols for certain vector indicies. This makes code involving
-- ranges of lines, and code involving translating user-facing @('Absolute' 'LineIndex')@ values to
-- simplliied arithmetic expressions that I consuder to be more human readable.

class MonadIO m => MonadEditVec vec m | m -> vec where
  nullElem  :: vec ~ v elem => m elem
  newVector :: Int -> m vec
  modVector :: (vec -> vec) -> m vec
  modCount  :: RelativeToCursor -> (Int -> Int) -> m Int
  cursorIndex :: RelativeToCursor -> m Int -- line editors and text editor have different cursors
  throwLimitErr :: RelativeToCursor -> m void
  throwIndexErr :: Int -> m void
  throwCountErr :: Int -> m void

instance MonadIO m => MonadEditVec (MVec.IOVector (TextLine tags)) (EditText tags m) where
  nullElem = pure TextLineUndefined
  newVector siz = stack ("newVector "++show siz) $ nullElem >>= liftIO . MVec.replicate siz
  modVector f = state $ \ st -> let vec = f $ theBufferVector st in
    (vec, st{ theBufferVector = vec })
  modCount rel f = state $ \ st -> case rel of
    Before -> let i = f $ theLinesAboveCursor st in
      (i, st{ theLinesAboveCursor =
                trace ("(TextLine.modCount Before -> "++show i++")") --DEBUG
                i
            })
    After  -> let i = f $ theLinesBelowCursor st in
      (i, st{ theLinesBelowCursor =
                trace ("(TextLine.modCount After -> "++show i++")") --DEBUG
                i
            })
  cursorIndex = peek "EditText.cursorIndex" . \ case
    Before -> subtract 1 <$> getElemCount Before
    After  -> cursorIndexAfter
  throwLimitErr = throwError . EndOfLineBuffer
  throwIndexErr = throwError . LineIndexOutOfRange . Absolute . LineIndex
  throwCountErr = throwError . LineCountOutOfRange . Relative . LineIndex

instance MonadIO m => MonadEditVec (UMVec.IOVector Char) (EditLine tags m) where
  nullElem = pure '\0'
  newVector siz = stack ("EditLine.newVector "++show siz) nullElem >>= liftIO . UMVec.replicate siz
  modVector f = state $ \ st -> let vec = f $ theLineEditBuffer st in
    (vec, st{ theLineEditBuffer = vec })
  modCount rel f = state $ \ st -> case rel of
    Before -> let i = f $ theCharsBeforeCursor st in
      (i, st{ theCharsBeforeCursor =
                trace ("(EditLine.modCount Before -> "++show i++")") --DEBUG
                i
            })
    After  -> let i = f $ theCharsAfterCursor  st in
      (i, st{ theCharsAfterCursor  =
                trace ("(EditLine.modCount After -> "++show i++")") --DEBUG
                i
            })
  cursorIndex rel = peek ("EditLine.cursorIndex "++show rel) $ case rel of
    Before -> getElemCount Before
    After  -> cursorIndexAfter
  throwLimitErr = throwError . EndOfCharBuffer
  throwIndexErr = throwError . CharIndexOutOfRange . Absolute . CharIndex
  throwCountErr = throwError . CharCountOutOfRange . Relative . CharIndex

stack :: Monad m => String -> m a -> m a --DEBUG
stack msg f = traceM ("(begin: "++msg++")") >> f <* traceM ("(_end_: "++msg++")") --DEBUG
--stack = const id

look :: (Monad m, Show a) => String -> m a -> m a --DEBUG
look msg f = --DEBUG
  traceM ("(begin: "++msg++")") >> f >>= \ a -> --DEBUG
  traceM ("(_end_: "++msg++") -> "++show a) >> return a --DEBUG
--look = const id

peek :: (Monad m, Show a) => String -> m a -> m a
peek msg f = f >>= \ a -> traceM ("("++msg++" -> "++show a++")") >> return a
--peek = const id

-- The vector of the text buffer.
getVector :: MonadEditVec vec m => m vec
getVector = modVector id

-- Returns the number of valid elements on or 'Before' the cursor, or 'After' the cursor.
getElemCount :: MonadEditVec vec m => RelativeToCursor -> m Int
getElemCount rel = peek ("getElemCount "++show rel) $ modCount rel id

-- The size of the buffer allocation
getAllocSize :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem) => m Int
getAllocSize = peek "getAllocSize" $ GMVec.length <$> getVector

-- The number of valid lines in the buffer @('getElemCount' 'Before' + 'getElemCount' 'After')@.
countElems :: MonadEditVec vec m => m Int
countElems = peek "countElems" $ (+) <$> getElemCount Before <*> getElemCount After

-- The number of lines in the buffer that are not valid, @(bufAllocSize - bufLineCount)@
getUnusedSpace :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem) => m Int
getUnusedSpace = peek "getUnusedSpace" $ subtract <$> countElems <*> getAllocSize

-- Like 'cursorIndex' but evaluates to an exception if the index is out of bounds.
vecIndexChk
  :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem)
  => RelativeToCursor -> m Int
vecIndexChk rel = peek ("vecIndexChk "++show rel) $ do
  siz <- getAllocSize
  i   <- cursorIndex rel
  if 0 <= i && i < siz then return i else throwLimitErr rel

-- Get the vector index for the element after the cursor, which is the first element after the
-- contiguous gap of undefined elements in the vector.
cursorIndexAfter :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem) => m Int
cursorIndexAfter = subtract <$> getElemCount After <*> getTopIndex

-- The top-most index, regardless of whether the index contains a valid line.
getTopIndex :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem) => m Int
getTopIndex = peek "getTopIndex" $ subtract 1 <$> getAllocSize

-- The index of the top-most valid line, which may not exist. Bounds checking is performed.
getTopIndexChk :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem) => m Int
getTopIndexChk = peek "getTopIndexChk" $ getElemCount After >>= \ i ->
  if i <= 0 then throwLimitErr After else getTopIndex

-- Get the index within the vector that is associated with the given 'LineIndex'. Bounds checking is
-- performed.
getAbsoluteChk :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem) => Absolute Int -> m Int
getAbsoluteChk (Absolute i) = peek ("getAbsoluteChk "++show i) $ getAllocSize >>= \ siz ->
  (if not $ 0 <= i && i < siz then throwIndexErr else getAbsolute . Absolute) i

-- Get the index within the vector that is associated with the given 'LineIndex'. Bounds checking is
-- __NOT__ performed.
getAbsolute :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem) => Absolute Int -> m Int
getAbsolute (Absolute i) = peek ("getAbsolute "++show i) $ getElemCount Before >>= \ cur ->
  if i <= cur then return i else (+ i) <$> getUnusedSpace

-- Convert a @('Relative' 'LineIndex')@ to an @('Absolute' 'LineIndex')@.
getRelToAbs
  :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem)
  => (Relative Int) -> m (Absolute Int)
getRelToAbs (Relative i) = peek ("getRelToAbs "++show i) $ Absolute . (+ i) <$> getElemCount Before

-- Return the starting and ending index of the void region of the buffer, which is the contiguous
-- region of undefined elements within the buffer.
getVoid
  :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem)
  => m (Maybe (Absolute Int, Absolute Int))
getVoid = peek "getVoid" $ do
  rgn <- (,) <$> (Absolute . (+ 1) <$> cursorIndex Before)
             <*> (Absolute . subtract 1 <$> cursorIndex After)
  return $ guard (uncurry (<=) rgn) >> Just rgn

-- True if there are no undefined text line elements in the buffer.
getIsFull
  :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem)
  => m Bool
getIsFull = peek "getIsFull" $ (== 0) <$> getUnusedSpace

-- Make a slice of elements relative to the current cursor. A negative argument will take elements
-- before and up-to the cursor, a positive argument will take that many elements starting from
-- 'getLineAfterCur'. Pass a boolean value indicating whether or not you would like to perform
-- bounds checking, if so an exception will be raised if the line index goes out of bounds.
getSlice
  :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem)
  => Relative Int -> m (vec st elem)
getSlice (Relative count) = stack ("getSlice "++' ':show count) $ do
  vec <- getVector
  if count < 0
   then do
    i <- (+ count) <$> getElemCount Before
    return $ trace ("(getSlice.slice "++show i++' ':show count++")") $ GMVec.slice i (abs count) vec
   else if count > 0
   then do
    i   <- cursorIndex After
    return $ trace ("(getSlice.slice "++show i++' ':show count++")") $ GMVec.slice i count vec
   else return $ GMVec.slice 0 0 vec

-- Like 'getSlice' but 
getSliceChk
  :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem)
  => Relative Int -> m (vec st elem)
getSliceChk rel@(Relative count) = do
  if count < 0
   then do
    bef <- getElemCount Before
    when (bef + count < 0) (throwCountErr count)
   else do
    aft <- cursorIndex After
    top <- getTopIndex
    when (aft + count > top) (throwCountErr count)
  getSlice rel

-- Make a slice of the void region of the buffer (the contiguous region of invalid elements after
-- the cursor). This can be used as the target of a vector copy. If the buffer is full, 'Nothing' is
-- returned.
getVoidSlice
  :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem)
  => Relative Int -> m (Maybe (vec st elem))
getVoidSlice (Relative count) = stack ("getVoidSlice "++show count) $ do
  vec <- getVector
  vsp <- getVoid
  return $ vsp <&> \ (Absolute lo, Absolute hi) ->
   if count < 0
   then trace ("(getVoidSlice.slice "++show (hi + count)++' ':show (abs count)++")") $ GMVec.slice (hi + count) (abs count) vec
   else if count > 0
   then trace ("(getVoidSlice.slice "++show lo++' ':show count++")") $ GMVec.slice lo count vec
   else GMVec.slice 0 0 vec

-- Obtain a slice (using 'getSlice') for the portion of the vector containing elements before or on
-- the current cursor.
getLoSlice :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem) => m (vec st elem)
getLoSlice = stack "getLoSlice" $ getElemCount Before >>= getSlice . Relative . negate

-- Obtain a slice (using 'getSliceM') for the portion of the vector containing elements after the
-- current cursor.
getHiSlice :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem) => m (vec st elem)
getHiSlice = stack "getHiSlice" $ getElemCount After >>= getSlice . Relative

-- Select a region of valid elements given an index and size values, taking into account the
-- position of the cursor, and copy the region into a new contiguous mutable vector that contains no
-- invalid elements.
copyRegion
  :: (GMVec.MVector vec elem,
       MonadEditVec (vec (PrimState m) elem) (t m), PrimMonad m,
       MonadTrans t
     )
  => Absolute Int -> Relative Int -> t m (vec (PrimState m) elem)
copyRegion (Absolute i) (Relative count) = stack ("copyRegion "++show i++' ':show count) $ do
  vec <- getVector
  bef <- getElemCount Before
  let aft = i + count
  if count == 0 then newVector 0
   else if i <  bef && aft <  bef then
    lift $ GMVec.clone $ GMVec.slice i count vec
   else if i <= bef && aft >= bef then
    GMVec.slice <$> getAbsolute (Absolute i) <*> pure count <*> pure vec >>= lift . GMVec.clone
   else if i <  bef && aft >= bef then do
    let dist = i - bef
    oldlo  <- getSlice $ Relative dist
    oldhi  <- getSlice $ Relative $ dist + count
    let oldlolen = GMVec.length oldlo
    let oldhilen = GMVec.length oldhi
    newvec <- lift $ GMVec.new $ oldlolen + oldhilen
    let newlo = GMVec.slice 0 oldlolen newvec
    let newhi = GMVec.slice oldlolen oldhilen newvec 
    let copy  = (.) lift . if unsafeMode then GMVec.unsafeCopy else GMVec.copy
    copy newlo oldlo
    copy newhi oldhi
    return newvec
   else error "copyRegion: internal error, this should never happen"

-- Write an element to a vector index, overwriting whatever was there before. __WARNING__: there is
-- no bounds checking.
putElemIndex
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem)
  => Int -> elem -> m ()
putElemIndex i elem = peek ("putElemIndex "++show i) $
  write <$> getVector <*> pure i <*> pure elem >>= liftIO where
    write = if unsafeMode then GMVec.unsafeWrite else GMVec.write

-- Read an element from a vector index. __WARNING__: there is no bounds checking.
getElemIndex
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem)
  => Int -> m elem
getElemIndex i = stack ("getElemIndex "++show i) $ read <$> getVector <*> pure i >>= liftIO where
  read = if unsafeMode then GMVec.unsafeRead else GMVec.read

-- Like 'putElemIndex' but the index to which the element is written is given by a
-- 'RelativeToCursor' value, so the index returned by 'cursorIndex'. The cursor position is not
-- modified.
putElem
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem)
  => RelativeToCursor -> elem -> m ()
putElem rel elem = peek ("putElem "++show rel) $ join $ putElemIndex <$> cursorIndex rel <*> pure elem

-- Like 'getElemIndex' but the index from which the element is read is given by a 'RelativeToCursor'
-- value, so the index returned by 'cursorIndex'. The cursor position is not modified.
getElem
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem)
  => RelativeToCursor -> m elem
getElem i = stack ("getElem "++show i) $ cursorIndex i >>= getElemIndex
  --cursorIndex >=> getElemIndex

-- Like 'putElem' but the @elem@ value to be put is taken from the 'nullElem' for this 'MonadEditVec'
-- context. The cursor position is not modified.
delElem
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem)
  => RelativeToCursor -> m ()
delElem rel = peek ("delElem "++show rel) $
  join (putElem rel <$> nullElem) >> void (modCount rel $ subtract 1)

-- Takes two paramters @oldSize@ and @newSize@. This function finds the minimum power of two scalara
-- necessary to scale the @oldSize@ such that it is __greater_than__ the @newSize@. Said another
-- way, this function repeatedly multiplies @oldSize@ by 2 until it is greater than (not equal to)
-- the @newSize@ value.
--
-- Note that this function does ever not return a value equal to @newSize@, the assumption is that
-- this function is used to grow a buffer, not to make it exactly the size required. If you want
-- this function to return a value equal to the exact amount, try passing a @newSize@ value equal to
-- @oldSize * 2^n - 1@ for some integer @n@.
minPow2ScaledSize :: Int -> Int -> Int
minPow2ScaledSize oldsiz newsiz = head $ dropWhile (<= newsiz) $ iterate (* 2) $ max 1 oldsiz

-- Pass a minimum vector allocation size request as an argument. This function check the allocation
-- size (using 'getAllocSize') of the current buffer vector. If the current allocation is larger
-- than the size request argument, 'Nothing' is returned. If the current allocation is smaller than
-- the size request, a new vector is allocated and returned in a 'Just' constructor. The new buffer
-- is ONLY allocated and returned; the content of the current buffer is NOT copied, the current
-- buffer is not replaced.
prepLargerVector
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem)
  => Int -> m (Maybe (vec RealWorld elem))
prepLargerVector newsiz = stack ("prepLargerVector "++show newsiz) $ do
  oldsiz <- getAllocSize
  if oldsiz >= newsiz then return Nothing else
    Just <$> newVector (minPow2ScaledSize oldsiz newsiz)

-- If and only if the vector is full, allocate a new vector with enough space to fit the requested
-- number of elements by doubling the size of the current vector until the size is large enough,
-- then copy the elements from the current vector to the new vector, and then replace the current
-- vector with the new one.
growVector
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem)
  => Int -> m ()
growVector increase = stack ("growVector "++show increase) $ if increase <= 0 then return () else do
  count  <- countElems
  prepLargerVector (count + increase) >>= \ case
    Nothing     -> return ()
    Just newvec -> do
      oldbef <- getLoSlice
      oldaft <- getHiSlice
      modVector $ const newvec
      newbef <- getLoSlice
      newaft <- getHiSlice
      liftIO $ do
        GMVec.copy newbef oldbef
        GMVec.copy newaft oldaft

-- Push a single element to the index 'Before' (currently on) the cursor, or the index 'After' the
-- cursor, and then shift the cursor to point to the pushed element.
pushElem
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem)
  => RelativeToCursor -> elem -> m ()
pushElem rel elem = stack ("pushElem "++show rel) $ do
  growVector 1
  let put = putElem rel elem
  let inc = void $ modCount rel (+ 1)
  case rel of { Before -> put >> inc; After -> inc >> put; }

-- Pop a single element from the index 'Before' (currently on) the cursor, or from the index 'After'
-- the cursor, and then shift the cursor to point to the pushed element.
popElem
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem)
  => RelativeToCursor -> m elem
popElem rel = stack ("popElem "++show rel) $ getElem rel >>= \ elem -> delElem rel >> return elem

-- Move the cursor, which will shift elements around the vector, using a algorithm of O(n) steps,
-- where @n@ is the 'Relative' shift value. Pass a boolean value indicating whether or not you would
-- like to perform bounds checking, if so an exception will be raised if the line index goes out of
-- bounds.
shiftCursor
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem)
  => Relative Int -> m ()
shiftCursor (Relative count) = stack ("shiftCursor "++show count) $ if count == 0 then return () else getVoid >>= \ case
  Nothing -> do
    siz <- getAllocSize
    cur <- getElemCount Before
    let i = cur + count
    if not $ 0 <= i && i < siz then throwCountErr count else void $ do
      modCount Before (+ count)
      modCount After  (+ count)
  Just (Absolute lo, Absolute hi) ->
    if      count ==  1 then popElem After  >>= pushElem Before
    else if count == -1 then popElem Before >>= pushElem After
    else do
      vec  <- getVector
      from <- getSlice $ Relative count -- necessary bounds checking is done in 'getSlice'
      let slice = if unsafeMode then GMVec.unsafeSlice else GMVec.slice
      let to = vec &
            if      count > 1 then trace ("(shiftCursor.slice "++show lo++' ':show (abs count)++")") $ slice lo count
            else if count < 1 then trace ("(shiftCursor.slice "++show (hi + count)++' ':show (negate count)++")") $ slice (hi + count) (negate count)
            else error "shiftCursor: internal error, this should never happen"
      liftIO $ GMVec.copy to from

-- Create a duplicate of the vector within.
dupVector :: (GMVec.MVector v a, MonadEditVec (v RealWorld a) m) => m (v RealWorld a)
dupVector = getVector >>= liftIO . GMVec.clone

-- Copy the current mutable buffer vector to an immutable vector.
freezeVector :: (GVec.Vector v a, MonadEditVec (GVec.Mutable v RealWorld a) m) => m (v a)
freezeVector = stack ("freezeVector") $ do
  newvec <- countElems >>= newVector
  bef <- getLoSlice
  aft <- getHiSlice
  let slice = if unsafeMode then GMVec.unsafeSlice else GMVec.slice
  liftIO $ do
    trace ("(freezeVector.slice 0 "++show (GMVec.length bef)++")") $ --DEBUG
      GMVec.copy (slice 0 (GMVec.length bef) newvec) bef
    trace ("(freezeVector.slice "++show (GMVec.length bef)++' ':show (GMVec.length aft)++")") $ --DEBUG
      GMVec.copy (slice (GMVec.length bef) (GMVec.length aft) newvec) aft
    GVec.unsafeFreeze newvec

----------------------------------------------------------------------------------------------------

-- | Throughout this module you will find functions that are defined like so:
--
-- @
-- insertChar :: 'MonadEditText' editor => RelativeToCursor -> Char -> editor tags ()
-- @
--
-- So you may wonder, when is it possible to use 'insertString' since it's type is the unspecified
-- @editor@ type variable?
--
-- There are two different function types which satisfy the @editor@ type variable: the 'EditText'
-- function type which is evaluated by the 'runEditText' function, and the 'TextFoldMap' function
-- type which is evaluated by the 'runFoldMapLines' function.
--
-- So any function type you see in this module that evaluates to a polymorphic type variable
-- @editor@, where @editor@ is a member of 'MonadEditText' (for example 'insertString'), can be used
-- when building either a 'EditText' or 'TextFoldMap' function that is then passed as a parameter to
-- the 'runEditText' or 'runFoldMapLines' (respectively).
--
-- This design pattern is similar to 'Control.Monad.IO.Class.liftIO', and then defining an API in
-- which all functions evaluate to a function of type @'Control.Monad.IO.Class.MonadIO' m => m a@,
-- which allows you to evaluate any of these API functions in any monadic function context (usually
-- a @do@ block of code) without needing to explicitly call 'liftIO', which is possible as long as
-- that monadic context is a member of the 'Control.Monad.IO.Class.MonadIO' typeclass. Likewise,
-- most API functions in this module can be evauated in any monadic context without needing to
-- explicitly call 'liftEditText'.
class MonadEditText m where
  liftEditText
    :: (MonadIO io
       , Show tags --DEBUG
       ) => EditText tags io a -> m tags io a

instance MonadEditText EditText where { liftEditText = id; }
instance MonadEditText (FoldMapLines r fold) where
  liftEditText = FoldMapLines . lift . lift . lift

-- | This class is basically the same as 'MonadEditText', but lifts a 'EditLine' function rather
-- than an 'EditText' function. Note that 'MonadEditText' is a subclass of this typeclass, which
-- means if you 'liftEditChar' should work anywhere a 'liftEditText' function will work.
class MonadEditLine m where
  liftEditLine
    :: (MonadIO io
       , Show tags --DEBUG
       )
    => EditLine tags io a -> m tags io a

instance MonadEditLine EditLine where { liftEditLine = id; }

instance MonadEditLine (FoldMapChars r fold) where
  liftEditLine = FoldMapChars . lift . lift . lift

instance MonadEditLine EditText where { liftEditLine = editLine; }

instance MonadEditLine (FoldMapLines r fold) where
  liftEditLine = foldMapChars . liftEditLine

----------------------------------------------------------------------------------------------------

-- | Controls whether characters are inserted/deleted before or after the cursor.
data RelativeToCursor = Before | After
  deriving (Eq, Ord, Read, Show, Bounded, Enum)

-- Not for export: This function takes a 'RelativeToCursor' value and constructs a lens that can be
-- used to access 'TextLine's within the 'LineEditor'.
relativeToLine :: RelativeToCursor -> Lens' (TextBufferState tags) Int
relativeToLine = \ case { Before -> linesAboveCursor; After -> linesBelowCursor; }
  -- This function may dissapear if I decide to buffer lines in a mutable vector.

-- Not for export: This function takes a 'RelativeToCursor' value and constructs a lens that can be
-- used to access a character index within the 'bufferLineEditor'.
relativeToChar :: RelativeToCursor -> Lens' (TextBufferState tags) Int
relativeToChar =
  (bufferLineEditor .) . \ case { Before -> charsBeforeCursor; After -> charsAfterCursor; }

-- Not for export: unsafe, requires correct accounting of cursor positions, otherwise segfaults may
-- occur. The cursor is implemented by keeping a mutable array in which elements before the cursor
-- fill the array from index zero up to the cursor, and the elements after the cursror fill the
-- array from the top-most index of the array down to the index computed from the top-most index of
-- the array subtracted by the total number of elements in the array plus the cursor position.
growVec
  :: (GMVec.MVector vector a, PrimMonad m)
  => vector (PrimState m) a
  -> Int -> Int -> Int -> m (vector (PrimState m) a)
growVec vec before after addElems = do
  let reqSize = before + after + addElems
  let len = GMVec.length vec
  if reqSize <= len then return vec else do
    let newSize = head $ dropWhile (< reqSize) $ iterate (* 2) len
    let slice = if unsafeMode then GMVec.slice else GMVec.unsafeSlice
    newVec <- GMVec.new newSize
    GMVec.copy (slice 0 before newVec) (slice 0 before vec)
    GMVec.copy (slice (len - after) after newVec) (slice (len - after) after vec)
    return newVec

-- Not for export: creates a deep-copy of a vector with a cursor, in which elements before the
-- cursor are aligned at the start of the vector, and elements after the cursor are aligned at the
-- end of the vector.
copyVec
  :: (GMVec.MVector vector a, PrimMonad m)
  => vector (PrimState m) a
  -> Int -> Int -> m (vector (PrimState m) a)
copyVec oldVec before after = do
  let len   = GMVec.length oldVec
  let upper = len - after
  let slice = if unsafeMode then GMVec.unsafeSlice else GMVec.slice
  newVec <- GMVec.new len
  when (before > 0) $ GMVec.copy (slice     0 before newVec) (slice     0 before oldVec)
  when (after  > 0) $ GMVec.copy (slice upper  after newVec) (slice upper  after oldVec)
  return newVec

-- Not for export: should be executed automatically by insertion operations. Increases the size of
-- the 'lineEditBuffer' to be large enough to contain the current 'lineEditorCharCount' plus the
-- given number of elements.
growLineIfTooSmall :: MonadIO m => Int -> EditText tags m ()
growLineIfTooSmall grow = do
  cur <- use bufferLineEditor
  buf <- liftIO $
    growVec (cur ^. lineEditBuffer) (cur ^. charsBeforeCursor) (cur ^. charsAfterCursor) grow
  bufferLineEditor . lineEditBuffer .= buf

growBufferIfTooSmall :: MonadIO m => Int -> EditText tags m ()
growBufferIfTooSmall grow = do
  buf   <- use bufferVector
  above <- use linesAboveCursor
  below <- use linesBelowCursor
  liftIO (growVec buf above below grow) >>= assign bufferVector

-- | Push a 'TextLine' before or after the cursor. This function does not effect the content of the
-- 'bufferLineEditor'.
pushLine
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags -- DEBUG
     )
  => RelativeToCursor -> TextLine tags -> editor tags m ()
pushLine rel line = liftEditText $ do
  growBufferIfTooSmall 1
  i <- cursorIndex rel --DEBUG
  traceM $ "pushLine: rel="++show rel++", cursorIndex="++show i++", line="++show line --DEBUG
  MVec.write <$> use bufferVector <*> cursorIndex rel <*> pure line >>= liftIO --TODO: replace with 'pushElem'
  relativeToLine rel += 1
  i <- cursorIndex rel --DEBUG
  traceM $ "pushLine: rel="++show rel++", cursorIndex="++show i --DEBUG

unsafePopLine
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => RelativeToCursor -> EditText tags m (TextLine tags)
unsafePopLine rel = do
  vec <- use bufferVector
  i   <- subtract 1 <$> cursorIndex rel
  traceM $ "unsafePopLine: rel="++show rel++", cursorIndex="++show i
  line <-  --DEBUG
    liftIO (MVec.read vec i <* MVec.write vec i TextLineUndefined) <* --TODO: replace with 'popElem'
      ((case rel of{ Before -> linesAboveCursor; After -> linesBelowCursor; }) -= 1)
  traceM $ "unsafePopLine: "++show line --DEBUG
  return line --DEBUG

-- | Pop a 'TextLine' from before or after the cursor. This function does not effect the content of
-- the 'bufferLineEditor'. If you 'popLine' from 'Before' the cursor when the 'bufferLineEditor'
-- is at the beginning of the buffer, or if you 'popLine' from 'After' the cursor when the
-- 'bufferLineEditor' is at the end of the buffer, 'Prelude.Nothing' will be returned.
popLine
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => RelativeToCursor -> editor tags m (Maybe (TextLine tags))
popLine = liftEditText . \ case
  Before -> use linesAboveCursor >>= \ before ->
    if before <= 0 then return Nothing else Just <$> unsafePopLine Before
  After  -> use linesBelowCursor >>= \ after ->
    if after <= 0 then return Nothing else Just <$> unsafePopLine After

----------------------------------------------------------------------------------------------------

indexToLine :: Int -> Absolute LineIndex
indexToLine = Absolute . LineIndex . (+ 1)

indexToChar :: Int -> Absolute CharIndex
indexToChar = Absolute . CharIndex . (+ 1)

countToLine :: Int -> Relative LineIndex
countToLine = Relative . LineIndex

countToChar :: Int -> Relative CharIndex
countToChar = Relative . CharIndex

lineToIndex :: Absolute LineIndex -> Int
lineToIndex (Absolute (LineIndex i)) = i - 1

charToIndex :: Absolute CharIndex -> Int
charToIndex (Absolute (CharIndex i)) = i - 1

lineToCount :: Relative LineIndex -> Int
lineToCount (Relative (LineIndex i)) = i

charToCount :: Relative CharIndex -> Int
charToCount (Relative (CharIndex i)) = i

----------------------------------------------------------------------------------------------------

-- Not for export: I don't want to encourage the use of a self-reference within an 'EditText'
-- function. It is already easy enough to cause a deadlock by evaluating 'runEditText' with 'liftIO'
-- within a 'runEditText' function.
--
-- Obtain a reference to the 'TextBuffer' that this was given to this function's monadic evaluator.
thisTextBuffer :: Monad m => EditText tags m (TextBuffer tags)
thisTextBuffer = EditText ask

-- | Get the current line number of the cursor.
currentLineNumber
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m (Absolute LineIndex)
currentLineNumber = liftEditText $ indexToLine <$> getElemCount Before

-- | Get the current column number of the cursor.
currentColumnNumber
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m (Absolute CharIndex)
currentColumnNumber = liftEditText $ editLine $ indexToChar <$> getElemCount Before

-- | Get the current cursor position.
currentTextLocation
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m TextLocation
currentTextLocation = TextLocation <$> currentLineNumber <*> currentColumnNumber

copyLineEditor'
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => EditLine tags m (TextLine tags)
copyLineEditor' = TextLine <$> freezeVector <*> use textCursorTags <*> use cursorBreakSize
  >>= (\ line -> trace ("(copyLineEditor -> "++show line++")") $ return line) --DEBUG

-- | Create a copy of the 'bufferLineEditor'.
copyLineEditorText
  :: (MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m (TextLine tags)
copyLineEditorText = liftEditLine copyLineEditor'

-- | Read a 'TextLine' from an @('Absolute' 'LineIndex')@ address.
readLineIndex
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Absolute LineIndex -> editor tags m (TextLine tags)
readLineIndex i = liftEditText $ getAbsoluteChk (Absolute $ lineToIndex i) >>= getElemIndex

-- | Write a 'TextLine' (as produced by 'copyLineEditorText' or readLineIndex') to an @('Absolute'
-- 'LineIndex')@ address.
writeLineIndex
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Absolute LineIndex -> TextLine tags -> editor tags m ()
writeLineIndex i line = liftEditText $
  getAbsoluteChk (Absolute $ lineToIndex i) >>= flip putElemIndex line

-- | If not already in 'bufferInsertMode', this function copies the 'TextLine' at the 'LineIndex'
-- given by 'linesAboveCursor' (i.e. it copies the line under the cursor) into the local
-- 'bufferLineEditor' line editor. Functions that evaluate to an 'EditLine' function type can then
-- perform a character-by-character edit on the 'bufferLineEditor' line editor. No 'TextLine's in
-- the 'TextBuffer' are modified or overwritten until 'endInsertMode' is evaluated.
--
-- This function returns 'True' if the insertion mode state was changed successfully. If the
-- 'TextBuffer' was already in insert mode when this function was called this function returns
-- 'False' because no state change is necessary if this function is called when the 'TextBuffer' is
-- already in insert mode.
beginInsertMode
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m Bool
beginInsertMode = look "beginInsertMode" $ liftEditText $ do
  insMode <- use bufferInsertMode
  if insMode then return False else do
    bufferInsertMode .= True
    before <- getElemCount Before
    after  <- getElemCount After
    when (before <= 0) $ pushElem Before =<<
      if after <= 0 then use bufferDefaultLine else popElem After
    before <- getElemCount Before
    elem   <- getElemIndex (before - 1)
    editLine $ join $
      editReplaceCurrentLine <$> (indexToChar <$> getElemCount Before) <*> pure elem
    return True

-- | If in 'bufferInsertMode', this function places the 'bufferLineEditor' back into the text
-- buffer, overwriting the 'TextLine' at the 'LineIndex' given by 'linesAboveCursor'. This function
-- then clears the 'bufferLineEditor' line editor.
--
-- It is possible to move the 'linesAboveCursor' to some other 'LineIndex' while in insert mode,
-- 'endInsertMode' does not care what the 'linesAboveCursor' was when 'beginInsertMode' was called,
-- it will always write the edited line to the 'LineIndex' given by 'linesAboveCursor'. So it is
-- possible to overwrite a 'TextLine' different from the line that was copied when 'beginInsertMode'
-- was evaluated.
--
-- This function returns 'True' if the insertion mode state was changed successfully. If the
-- 'TextBuffer' was not in insert mode when this function was called this function returns 'False'
-- because no state change is necessary if this function is called when the 'TextBuffer' is already
-- not in insert mode.
endInsertMode
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m Bool
endInsertMode = look "endInsertMode" $ liftEditText $ do
  insMode <- use bufferInsertMode
  if not insMode then return False else do
    (editLine copyLineEditor' <* clearCurrentLine) >>= putElem Before
    bufferInsertMode .= False
    return True

-- | Replace the content in the 'bufferLineEditor' with the content in the given 'TextLine'. Pass
-- an integer value indicating where the cursor position should be set. This function does not
-- re-allocate the current line editor buffer unless it is too small to hold all of the characters
-- in the given 'TextLine', meaning this function only grows the buffer memory allocation, it never
-- shrinks the memory allocation.
replaceCurrentLine
  :: (MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Absolute CharIndex -> TextLine tags -> editor tags m ()
replaceCurrentLine i = \ case
  TextLineUndefined -> return ()
  line              -> liftEditLine $ editReplaceCurrentLine i line

-- not for export
--
-- Identical to 'replaceCurrentLine' but does not call 'liftEditLine', which calls 'editLine', which
-- calls 'beginInsertMode', which calls 'replaceCurrentLine'.
editReplaceCurrentLine
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Absolute CharIndex -> TextLine tags -> EditLine tags m ()
editReplaceCurrentLine cur' line = stack ("editReplaceCurrentLine "++show cur'++' ':show line) $ do
  let srcvec = theTextLineString line
  let srclen = intSize srcvec
  let lbrksz = theTextLineBreakSize line
  let srctop = srclen - fromIntegral lbrksz
  let cur    = max 0 $ min srctop $ charToIndex cur'
  join $ maybe (pure ()) (void . modVector . const) <$> prepLargerVector srclen
  targlo <- getLoSlice
  targhi <- getHiSlice
  let targlolen = UMVec.length targlo
  let targhilen = UMVec.length targhi
  charsBeforeCursor .= targlolen
  charsAfterCursor  .= targhilen
  cursorBreakSize   .= lbrksz
  textCursorTags    .= theTextLineTags line
  liftIO $ do
    let slice = if unsafeMode then UVec.unsafeSlice else UVec.slice
    when (cur > 0) $ UVec.copy targlo $ trace ("(editReplaceCurrentLine.slice 0"++show targlolen++")") $ slice 0 targlolen srcvec
    UVec.copy targhi $ trace ("(editReplaceCurrentLine.slice "++show targlolen++' ':show targhilen++")") $ slice targlolen targhilen srcvec

-- | Delete the content of the 'bufferLineEditor' except for the line breaking characters (if any)
-- at the end of the line. This function does not change the memory allocation for the 'LineEditor',
-- it simply sets the character count to zero. Tags on this line are not effected.
clearCurrentLine
  :: (MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m ()
clearCurrentLine = stack "clearCurrentLine" $ liftEditLine $ do
  charsBeforeCursor .= 0
  use cursorBreakSize >>= assign charsAfterCursor . fromIntegral

-- | Like 'clearCurrentLine', this function deletes the content of the 'bufferLineEditor', and tags
-- on this line are not effected. The difference is that this function replaces the
-- 'bufferLineEditor' with a new empty line, resetting the line editor buffer to the default
-- allocation size and allowing the garbage collector to delete the previous allocation. This means
-- the line editor buffer memory allocation may be shrunk to it's minimal/default size.
resetCurrentLine
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m ()
resetCurrentLine = liftEditText $ newLineEditor >>= assign bufferLineEditor

-- | Create a new 'LineEditor' value within the current 'EditText' context, using the default tags
-- given by 'bufferDefaultTags'. This function calls 'newLineEditorIO' using the value of
-- 'textCursorTags'.
newLineEditor
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m (LineEditor tags)
newLineEditor = liftEditText $ thisTextBuffer >>= \ this ->
  use (bufferLineEditor . textCursorTags) >>= liftIO . newLineEditorIO this

-- | Create a copy of the current 'lineEditBuffer' and return it. This function calls
-- 'copyLineEditorIO' using the value of the current 'lineEditBuffer'.
copyLineEditor
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m (LineEditor tags)
copyLineEditor = stack "copyLineEditor" $ liftEditText $ use bufferLineEditor >>= liftIO . copyLineEditorIO

-- | This function calls 'moveByLine' and then 'moveByChar' to move the cursor by a number of lines
-- and characters relative to the current cursor position.
moveCursor
  :: (MonadEditText editor, MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Relative LineIndex -> Relative CharIndex -> editor tags m ()
moveCursor row col = moveByLine row >> moveByChar col

-- | Move the cursor to a different line by an @n :: Int@ number of lines. A negative @n@ indicates
-- moving the cursor toward the start of the buffer, a positive @n@ indicates moving the cursor
-- toward the end of the buffer.
moveByLine
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Relative LineIndex -> editor tags m ()
moveByLine = liftEditText . shiftCursor . Relative . lineToCount

-- | Move the cursor to a different character position within the 'bufferLineEditor' by an @n ::
-- Int@ number of characters. A negative @n@ indicates moving toward the start of the line, a
-- positive @n@ indicates moving toward the end of the line.
moveByChar
  :: (MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Relative CharIndex -> editor tags m ()
moveByChar = liftEditLine . shiftCursor . Relative . charToCount

-- | Go to an absolute line number, the first line is 1 (line 0 and below all send the cursor to
-- line 1), the last line is 'Prelude.maxBound'.
gotoLine
  :: (MonadEditText editor, MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Absolute LineIndex -> editor tags m ()
gotoLine (Absolute (LineIndex n)) = liftEditText $ do
  bef <- getElemCount Before
  moveByLine $ Relative $ LineIndex $ n - bef

-- | Go to an absolute character (column) number, the first character is 1 (character 0 and below
-- all send the cursor to column 1), the last line is 'Prelude.maxBound'.
gotoChar
  :: (MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Absolute CharIndex -> editor tags m ()
gotoChar (Absolute (CharIndex n)) = liftEditLine $ do
  before    <- use charsBeforeCursor
  moveByChar $ Relative $ CharIndex $ n - before

-- | This function calls 'gotoLine' and then 'gotoChar' to move the cursor to an absolute a line
-- number and characters (column) number.
gotoPosition
  :: (MonadEditText editor, MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => TextLocation -> editor tags m ()
gotoPosition (TextLocation{theLocationLineIndex=line,theLocationCharIndex=col}) =
  liftEditText $ gotoLine line >> gotoChar col

-- Not for export: does not filter line-breaking characters.
unsafeInsertString :: MonadIO m => RelativeToCursor -> CharVector -> EditText tags m ()
unsafeInsertString rel str = do
  let strlen = intSize str
  growLineIfTooSmall strlen
  buf <- use $ bufferLineEditor . lineEditBuffer
  off <- use $ relativeToChar rel
  let len = UMVec.length buf
  let i0  = case rel of { Before -> off; After -> len - off - strlen; }
  relativeToChar rel += strlen
  liftIO $ forM_ (zip [0 ..] $ unpack str) $ \ (i, c) -> UMVec.write buf (i0 + i) c

-- | Insert a single character. If the 'lineBreakPredicate' function evaluates to 'Prelude.True',
-- meaning the given character is a line breaking character, this function does nothing. To insert
-- line breaks, using 'insertString'. Returns the number of characters added to the buffer.
insertChar
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => RelativeToCursor -> Char -> editor tags m (Relative CharIndex)
insertChar rel c = liftEditText $ Relative . CharIndex <$> do
  isBreak <- use $ bufferLineBreaker . lineBreakPredicate
  if isBreak c then return 0 else liftEditLine (pushElem rel c) >> return 1

-- | This function only deletes characters on the current line, if the cursor is at the start of the
-- line and you evaluate @'deleteChars' 'Before'@, this function does nothing. This function does
-- not delete line breaking characters. Returns the number of characters actually deleted as a
-- negative number (or zero), indicating a change in the number of characters in the buffer.
deleteChars
  :: (MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Relative CharIndex -> editor tags m (Relative CharIndex)
deleteChars (Relative (CharIndex n)) = liftEditLine $ fmap (Relative . CharIndex) $
  if n < 0 then do
    before <- getElemCount Before
    let count = negate $ min before $ abs n 
    modCount Before $ const $ before + count
    return count
  else if n > 0 then do
    lbrksz <- fromIntegral <$> use cursorBreakSize
    after  <- getElemCount After
    let count = negate $ min n $ max 0 $ after - lbrksz
    modCount Before $ const $ after + count
    return count
  else return 0

-- | This function deletes characters starting from the cursor, and if the number of characters to
-- be deleted exceeds the number of characters in the current line, characters are deleted from
-- adjacent lines such that the travel of deletion wraps to the end of the prior line or the
-- beginning of the next line, depending on the direction of travel.
deleteCharsWrap
  :: ( MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Relative CharIndex -> editor tags m (Relative CharIndex)
deleteCharsWrap request = liftEditText $ if request == 0 then return 0 else
  let direction = if request < 0 then Before else After in
  -- First, delete the chararacters in the line editor, see if that satisfies the request...
  deleteChars request >>= \ delcount -> if abs delcount >= abs request then return delcount else
  -- otherwise ues 'forLines' to delete each line until the request has been satsified.
  fmap snd $ forLines direction (abs request + delcount, delcount) $ \ halt line ->
    get >>= \ st@(request, delcount) ->
    if request <= 0 then halt st else
    let weight = countToChar $ textLineWeight line in
    if weight <= request then
      put (request - weight, delcount - countToChar (intSize line)) >> return []
     else do
      case direction of
        Before -> do
          let len = countToChar (intSize line)
                  - fromIntegral (theTextLineBreakSize line) - weight + 1
          liftEditText $ unsafeInsertString After $
            UVec.slice 0 (charToCount len) $ theTextLineString line
          put (request - weight, delcount - len)
        After  -> do
          liftEditText $ unsafeInsertString Before $
            UVec.slice (charToCount weight) (intSize line - charToCount weight)
            (theTextLineString line)
          put (request - weight, delcount - weight)
      pure <$> liftEditLine copyLineEditorText
        

-- | This function evaluates the 'lineBreaker' function on the given string, and beginning from the
-- current cursor position, begins inserting all the lines of text produced by the 'lineBreaker'
-- function.
insertString
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => String -> editor tags m (Relative CharIndex)
insertString str = look ("insertString "++show str) $ liftEditText $ do
  breaker <- use (bufferLineBreaker . lineBreaker)
  let writeStr =
        look "insertString.writeStr" . --DEBUG
        editLine . fmap sum . mapM ((>> (return 1)) . pushElem Before)
        . (\ str -> trace ("(insertString.writeStr "++show str++")") str) --DEBUG
  let writeLine line@(str, lbrk) = look ("writeLine "++show line) $ do
        (strlen, lbrklen) <- (,) <$> writeStr str <*> writeStr lbrk
        traceM $ "(insertString.writeLine -> strlen="++show strlen++", lbrklen="++show lbrklen++")"
        let maxlen = fromIntegral (maxBound :: Word16) :: Int
        if lbrklen >= maxlen
         then error $ "insertString: line break string length is "++show lbrklen++
                "exceeeds maximum length of "++show maxlen++" characters"
         else do
          editLine $ cursorBreakSize .= fromIntegral lbrklen
          editLine copyLineEditor' >>= pushElem Before
          editLine clearCurrentLine
          return $ strlen + lbrklen
  let loop count = seq count . \ case
        []        -> return count
        line:more -> writeLine line >>= flip loop more
  result <- Relative . CharIndex <$> case breaker str of
    []    -> trace "(insertString.breaker -> [])" $ return 0
    lines -> trace ("(insertString.breaker -> "++show lines++")") $ loop 0 lines
  return result

-- Not for export: this code shared by 'forLinesInRange' and 'forLines' but requires knowledge of
-- the 'TextBuffer' internals in order to use, so is not something end users should need to know
-- about.
forLinesLoop
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => fold
  -> (FoldMapLinesHalt void fold tags m fold ->
      TextLine tags -> FoldMapLines fold fold tags m [TextLine tags])
  -> Int
  -> (EditText tags m (TextLine tags), TextLine tags -> FoldMapLines fold fold tags m ())
  -> EditText tags m fold
forLinesLoop fold f count (pop, push) = execFoldMapLines (callCC $ loop count) fold where
  loop count halt = if count <= 0 then get else
    liftEditText pop >>= f halt >>= mapM_ push >> loop (count - 1) halt

-- | This function moves the cursor to the first @'Absolute' 'LineIndex'@ parameter given (will
-- evaluate 'endInsertMode)', then evaluate a folding and mapping monadic function over a range of
-- lines specified. If the first 'LineIndex' parameter is greater than the second 'LineIndex'
-- parameter, the fold map operation evaluates in reverse line order.
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
forLinesInRange
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Absolute LineIndex -> Absolute LineIndex
  -> fold
  -> (FoldMapLinesHalt void fold tags m fold ->
      TextLine tags -> FoldMapLines fold fold tags m [TextLine tags])
  -> EditText tags m fold
forLinesInRange absFrom@(Absolute (LineIndex from)) (Absolute (LineIndex to)) fold f = do
  endInsertMode
  gotoLine absFrom
  lineCount <- (+) <$> use linesAboveCursor <*> use linesBelowCursor
  let dist = to - from
  forLinesLoop fold f (min lineCount . max 1 $ abs dist) $ if dist < 0
    then (unsafePopLine Before, pushLine After)
    else (unsafePopLine After,  pushLine Before)

-- | Conveniently calls 'forLinesInRange' with the first two parameters as @('Absolute' 1)@ and
-- @('Absolute' 'maxBound')@.
forLinesInBuffer
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => fold
  -> (FoldMapLinesHalt void fold tags m fold ->
      TextLine tags -> FoldMapLines fold fold tags m [TextLine tags])
  -> EditText tags m fold
forLinesInBuffer = forLinesInRange (Absolute 1) maxBound

-- | Like 'forLinesInRange', but this function takes a 'RelativeToCursor' value, iteration begins
-- at the 'bufferLineEditor', and if the 'RelativeToCursor' value is 'After' then iteration goes
-- forward to the end of the buffer, whereas if the 'RelativeToCursor' value is 'Before' then
-- iteration goes backward to the start of the buffer.
forLines
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => RelativeToCursor
  -> fold
  -> (FoldMapLinesHalt void fold tags m fold ->
      TextLine tags -> FoldMapLines fold fold tags m [TextLine tags])
  -> EditText tags m fold
forLines rel fold f = do
  above <- use linesAboveCursor
  below <- use linesBelowCursor
  uncurry (forLinesLoop fold f) $ case rel of
    Before -> (above, (unsafePopLine Before, pushLine After))
    After  -> (below, (unsafePopLine After, pushLine Before))

-- | Use the 'defaultLineBreak' value to break the line at the current cursor position.
lineBreak
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => RelativeToCursor -> editor tags m ()
lineBreak rel = liftEditText $ do
  breaker <- use bufferLineBreaker
  let lbrk     = breaker ^. defaultLineBreak
  let lbrkSize = fromIntegral $ intSize lbrk
  case rel of
    Before -> do
      unsafeInsertString Before lbrk
      cursor <- use bufferLineEditor
      let vec = cursor ^. lineEditBuffer
      let cur = cursor ^. charsBeforeCursor
      str <- liftIO $! UVec.freeze $! UMVec.slice 0 cur vec
      pushLine Before $ TextLine
        { theTextLineTags      = cursor ^. textCursorTags
        , theTextLineString    = str
        , theTextLineBreakSize = lbrkSize
        }
      bufferLineEditor . charsBeforeCursor .= 0
    After  -> do
      cursor <- use bufferLineEditor
      let vec = cursor ^. lineEditBuffer
      let len = UMVec.length vec
      let cur = cursor ^. charsAfterCursor
      when (cur > 0) $ do
        str <- liftIO $! UVec.freeze $! UMVec.slice (len - cur) cur vec
        pushLine After $ TextLine
          { theTextLineTags      = cursor ^. textCursorTags
          , theTextLineString    = str
          , theTextLineBreakSize = lbrkSize
          }
        bufferLineEditor . charsAfterCursor .= 0
      unsafeInsertString After lbrk

----------------------------------------------------------------------------------------------------

data TextLocation
  = TextLocation
    { theLocationLineIndex :: !(Absolute LineIndex)
    , theLocationCharIndex :: !(Absolute CharIndex)
    }
  deriving (Eq, Ord)

instance Show TextLocation where
  show (TextLocation
        { theLocationLineIndex=(Absolute (LineIndex line))
        , theLocationCharIndex=(Absolute (CharIndex char))
        }) = "TextLocation "++show line++' ':show char

relativeLine :: RelativeToCursor -> Int -> Relative LineIndex
relativeChar :: RelativeToCursor -> Int -> Relative CharIndex
(relativeLine, relativeChar) = (f LineIndex, f CharIndex) where
  f constr = ((Relative . constr) .) . \ case { Before -> negate; After -> id; }

lineIndex :: Lens' TextLocation (Absolute LineIndex)
lineIndex = lens theLocationLineIndex $ \ a b -> a{ theLocationLineIndex = b }

charIndex :: Lens' TextLocation (Absolute CharIndex)
charIndex = lens theLocationCharIndex $ \ a b -> a{ theLocationCharIndex = b }

-- | Get the current cursor location.
getCursor
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m TextLocation
getCursor = liftEditText $ TextLocation
  <$> (Absolute . LineIndex . (+ 1) <$> use linesAboveCursor)
  <*> editLine (Absolute . CharIndex . (+ 1) <$> use charsBeforeCursor)

-- | Move the cursor to given 'TextLocation'.
gotoCursor
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => TextLocation -> editor tags m ()
gotoCursor (TextLocation{theLocationLineIndex=line,theLocationCharIndex=char}) = liftEditText $
  gotoLine line >> editLine (gotoChar char)

-- | Save the location of the cursor, then evaluate an @editor@ function. After evaluation
-- completes, restore the location of the cursor (within range, as the location may no longer exist)
-- and return the result of evaluation.
saveCursorEval
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m a -> editor tags m a
saveCursorEval f = do
  (cur, a) <- (,) <$> getCursor <*> f
  gotoCursor cur >> return a

class RelativeToAbsoluteCursor index where
  -- | Convert a 'Relative' index (either a 'LineIndex' or 'CharIndex') to an 'Absolute' index.
  relativeToAbsolute
    :: (MonadEditText editor, Monad (editor tags m), MonadIO m
       , Show tags --DEBUG
       )
    => Relative index -> editor tags m (Absolute index)

instance RelativeToAbsoluteCursor LineIndex where
  relativeToAbsolute (Relative (LineIndex i)) = liftEditText $
    Absolute . LineIndex . (+ 1) . max 0 . app . (min &&& (+ i)) <$>
    use linesAboveCursor

instance RelativeToAbsoluteCursor CharIndex where
  relativeToAbsolute (Relative (CharIndex i)) = liftEditText $
    Absolute . CharIndex . (+ 1) . max 0 . app . (min &&& (+ i)) <$>
    liftEditLine (use charsBeforeCursor)

----------------------------------------------------------------------------------------------------

-- | 'TextView's are a shallow, immutable snapshot of all, or a portion of, a 'TextBuffer'. The
-- internal structure of a 'TextView' is identical to that of the 'TextBuffer' so no additional
-- conversion or copying is necessary, making the creation of views extremely fast. Create a
-- 'TextView' using 'textViewOnLines' or 'textViewOnRange'. Once a 'TextView' is created other
-- threads may immediately resume performing updates on the 'TextBuffer'.
data TextView tags
  = TextView
    { textViewCharCount :: !Int
      -- ^ Evaluates to how many characters exist in this buffer.
    , textViewVector    :: !(Vec.Vector (TextLine tags))
      -- ^ Evaluates to a vector of 'TextLine's so you can make use of the 'Vec.Vector' APIs to
      -- define your own operations on a 'TextView'.
    }

instance Semigroup tags => Semigroup (TextView tags) where
  (<>) = textViewAppend (<>)

instance Monoid tags => Monoid (TextView tags) where
  mempty  = TextView{ textViewCharCount = 0, textViewVector = mempty }
  mappend = textViewAppend mappend

newtype FoldTextView r fold tags m a
  = FoldTextView
    { unwrapFoldTextView :: ContT r (StateT fold m) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => MonadState fold (FoldTextView r fold tags m) where
  state = FoldTextView . lift . state

instance Monad m => MonadCont (FoldTextView r fold tags m) where
  callCC f = FoldTextView $ callCC $ unwrapFoldTextView . f . (FoldTextView .)

instance MonadTrans (FoldTextView r fold tags) where
  lift = FoldTextView . lift . lift

type FoldTextViewHalt void fold tags m r = r -> FoldTextView r fold tags m void

-- | An empty 'TextView', containing zero lines of text.
emptyTextView :: TextView tags
emptyTextView = TextView{ textViewCharCount = 0, textViewVector = Vec.empty }

-- | This function works similar to 'Data.Monoid.mappend' or the @('Data.Semigroup.<>')@ operator,
-- but allows you to provide your own appending function for the @tags@ value. Tags are appended if
-- the final lines of the left 'TextView' has no line break and therefore must to be prepended to
-- the first line of the right 'TextView'. If the left 'TextView' ends with a line break, the @tags@
-- appending function is never evaluated.
textViewAppend :: (tags -> tags -> tags) -> TextView tags -> TextView tags -> TextView tags
textViewAppend appendTags
  TextView{textViewCharCount=countA,textViewVector=vecA}
  TextView{textViewCharCount=countB,textViewVector=vecB} = TextView
    { textViewCharCount = countA + countB
    , textViewVector = let { lenA = Vec.length vecA; lenB = Vec.length vecB; } in
        if lenA == 0 then vecB else
        if lenB == 0 then vecA else Vec.create
          (do let lastA  = vecA Vec.! (lenA - 1)
              let firstB = vecB Vec.! 0
              let lenAB  = lenA + lenB
              let lineAB = TextLine
                    { theTextLineString = case lastA of
                        TextLineUndefined -> case firstB of
                          TextLineUndefined -> mempty
                          firstB            -> theTextLineString firstB
                        lastA             -> case firstB of
                          TextLineUndefined -> theTextLineString lastA
                          firstB            -> theTextLineString lastA <> theTextLineString firstB
                    , theTextLineTags = appendTags (theTextLineTags lastA) (theTextLineTags firstB)
                    , theTextLineBreakSize = theTextLineBreakSize firstB
                    }
              let listA = Vec.toList $ Vec.slice 0 (lenA - 1) vecA
              let listB = Vec.toList $ Vec.slice 1 (lenB - 1) vecB
              let (size, list) = if theTextLineBreakSize lastA > 0
                    then (lenAB, listA ++ lastA : firstB : listB)
                    else (lenAB - 1, listA ++ lineAB : listB)
              newVec <- MVec.new size
              forM_ (zip [0 ..] list) $ uncurry $ MVec.write newVec
              return newVec
          )
    }

-- | Create a 'TextView' from the content of a 'TextBuffer' in the given range delimited by the two
-- given 'TextLocation' values.
textView
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => TextLocation -> TextLocation -> TextBuffer tags
  -> m (Either TextEditError (TextView tags))
textView from to = stack ("textView ("++show from++") ("++show to++")") . liftIO . runEditTextIO (mkview (min from to) (max from to)) where
  mkview from to = countElems >>= \ nmax -> if nmax <= 0 then return emptyTextView else do
    copyLineEditorText >>= putElem Before
    let unline = max 0 . min (nmax - 1) . lineToIndex . theLocationLineIndex
    let (lo, hi) = (unline from, unline to)
    newvec <- copyRegion (Absolute lo) $ Relative $ 1 + hi - lo
    let top = MVec.length newvec - 1
    let unchar len lbrksz = max 0 . min (len - lbrksz) . charToIndex . theLocationCharIndex
    let onvec i f = liftIO $ MVec.read newvec i >>= MVec.write newvec i . \ case
          line@TextLine{}   ->
            let vec      = line ^. textLineString
                veclen   = UVec.length vec
                lbrksz   = fromIntegral $ theTextLineBreakSize line
                (i, len) = f veclen lbrksz
            in line & textLineString %~ UVec.slice i len
          TextLineUndefined -> error $
            "textView: trimmed vector contains undefined line at index "++show i
    onvec top $ \ len lbrksz -> (0, unchar len lbrksz to) 
    onvec 0   $ \ len lbrksz -> let i = unchar len lbrksz from in (i, len - i)
    let freeze = liftIO . if unsafeMode then Vec.unsafeFreeze else Vec.freeze
    newvec <- freeze newvec
    return TextView
      { textViewCharCount = sum $ Vec.toList newvec >>= \ case
          TextLineUndefined               -> []
          TextLine{theTextLineString=vec} -> [UVec.length vec]
      , textViewVector    = newvec
      }

-- | Like 'textView', creates a new text view, but rather than taking two 'TextLocation's to delimit
-- the range, takes two @('Absolute' 'LineIndex')@ values to delimit the range.
textViewOnLines
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Absolute LineIndex -> Absolute LineIndex
  -> TextBuffer tags -> m (Either TextEditError (TextView tags))
textViewOnLines from to = textView
  (TextLocation
   { theLocationLineIndex = min from to
   , theLocationCharIndex = Absolute $ CharIndex 0
   })
  (TextLocation
   { theLocationLineIndex = max from to
   , theLocationCharIndex = Absolute $ CharIndex maxBound
   })

-- | Create a new, editable 'TextBuffer' from a read-only 'TextView'. Pass 'Before' to place the
-- text in the buffer before the cursor, meaning the cursor will start positioned at the end of the
-- editable 'TextBuffer', or pass 'After' to place the text in the buffer after the cursor, meaning
-- the cursor will start at the beginning of the editable 'TextBuffer'.
newTextBufferFromView :: MonadIO m => RelativeToCursor -> tags -> TextView tags -> m (TextBuffer tags)
newTextBufferFromView rel tags (TextView{textViewVector=vec}) = liftIO $ do
  mvar <- newEmptyMVar
  let this = TextBuffer mvar
  let oldLen = Vec.length vec
  st0 <- newTextBufferState this (max 16 $ oldLen * 2) tags
  let newBuf = theBufferVector st0
  let newLen = MVec.length newBuf
  let cursor = if oldLen == 0 then 0 else
        if theTextLineBreakSize (vec Vec.! (oldLen - 1)) > 0 then oldLen else oldLen - 1
  let (idx, st) = case rel of
        Before -> ([0 ..], st & linesAboveCursor .~ cursor)
        After  -> ([newLen - oldLen ..], st & linesBelowCursor .~ cursor)
  forM_ (zip idx $ Vec.toList vec) $ uncurry $ MVec.write newBuf
  putMVar mvar st
  return this

-- | Perform a fold over every 'TextLine' in the 'TextView' using a function of type 'FoldTextView'.
forLinesInView
  :: Monad m
  => TextView tags -> fold
  -> (FoldTextViewHalt void fold tags m fold -> TextLine tags -> FoldTextView fold fold tags m ())
  -> m fold
forLinesInView (TextView{textViewVector=vec}) fold f = flip execStateT fold $
  runContT (unwrapFoldTextView $ callCC $ loop $ Vec.toList vec) return where
    loop lines halt = case lines of
      []         -> get
      line:lines -> f halt line >> loop lines halt

----------------------------------------------------------------------------------------------------

ralign :: Int -> String
ralign n = case abs n of
  i | i < 10 -> "      " ++ show i
  i | i < 100 -> "     " ++ show i
  i | i < 1000 -> "    " ++ show i
  i | i < 10000 -> "   " ++ show i
  i | i < 100000 -> "  " ++ show i
  i | i < 1000000 -> " " ++ show i
  i                ->       show i

-- | Print debugger information about the structured data that forms the 'TextView' to standard
-- output. __WARNING:__ this print's every line of text in the view, so if your text view has
-- thousands of lines of text, there will be a lot of output.
debugPrintView :: (Show tags, MonadIO m) => TextView tags -> m ()
debugPrintView view = do
  (_, errCount) <- forLinesInView view (1 :: Int, 0 :: Int) $ \ _halt line -> do
    lineNum <- state $ \ (lineNum, errCount) ->
      (lineNum, (lineNum + 1, errCount + if textLineIsUndefined line then 1 else 0))
    liftIO $ putStrLn $ ralign lineNum ++ ": " ++ show line
  liftIO $ putStrLn ""
  when (errCount > 0) $ error $ "iterated over "++show errCount++" undefined lines"

-- | Print debugger information about the structured data that forms the 'TextBuffer' to standard
-- output. __WARNING:__ this print's every line of text in the buffer, so if your text buffer has
-- thousands of lines of text, there will be a lot of output.
debugPrintBuffer
  :: (MonadEditText editor, Show tags, MonadIO (editor tags m), MonadIO m)
  => editor tags m ()
debugPrintBuffer = liftEditText $ do
  lineVec <- use bufferVector
  let len = MVec.length lineVec
  let printLines nullCount i = if i >= len then return () else do
        line <- liftIO $ MVec.read lineVec i
        let showLine = putStrLn $ ralign i ++ ": " ++ show line
        if textLineIsUndefined line
          then do
            liftIO $ if nullCount < (1 :: Int) then showLine else
              when (nullCount < 4) $ putStrLn "...."
            (printLines $! nullCount + 1) $! i + 1
          else liftIO showLine >> (printLines 0 $! i + 1)
  printLines 0 0
  above   <- use linesAboveCursor
  below   <- use linesBelowCursor
  insmode <- use bufferInsertMode
  liftIO $ do
    putStrLn $ "   linesAboveCursor: " ++ show above
    putStrLn $ "   linesBelowCursor: " ++ show below
    putStrLn $ "    bufferLineCount: " ++ show (above + below)
    putStrLn $ " __bufferInsertMode: " ++ show insmode

-- | The 'bufferLineEditor', which is a 'LineEditor' is a separate data structure contained within
-- the 'TextBuffer', and it is often not necessary to know this information when debugging, so you
-- can print debugging information about the 'LineEditor' by evaluating this function whenever it is
-- necessary.
debugPrintCursor
  :: (MonadEditText editor, Show tags, MonadIO (editor tags m), MonadIO m)
  => editor tags m ()
debugPrintCursor = liftEditText $ do
  cur <- use bufferLineEditor
  let charVec    = theLineEditBuffer cur
  let charVecLen = UMVec.length charVec
  let before     = cur ^. charsBeforeCursor
  let after      = cur ^. charsAfterCursor
  liftIO $ do
    str <- forM [0 .. charVecLen - 1] $ UMVec.read charVec
    putStrLn $ "     bufferLineEditor: " ++ show str
    putStrLn $ "       textCursorTags: " ++ show (cur ^. textCursorTags)
    putStrLn $ " __cursorVectorLength: " ++ show charVecLen
    putStrLn $ "    charsBeforeCursor: " ++ show before
    putStrLn $ "     charsAfterCursor: " ++ show after
    putStrLn $ "      cursorInputSize: " ++ show (before + after)
    putStrLn $ "  cursorLineBreakSize: " ++ show (theCursorBreakSize cur)
