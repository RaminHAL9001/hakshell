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
-- The 'TextBuffer' contains a local mutable line editor called a 'TextCursor'. Any single line from
-- within the buffer can be copied into the local 'TextCursor' using 'beginInsertMode'. The cursor
-- can be moved around using 'gotoChar' or 'moveByChar', characters can be inserted or deleted, then
-- the content of the 'TextCursor' can be copied back to the 'TextBuffer' using 'endInsertMode'.
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
    copyCurrentLine, replaceCurrentLine,
    pushLine, popLine,
    readLineIndex, writeLineIndex,
    forLines, forLinesInRange, forLinesInBuffer,
    -- * Text Views
    TextView, textView, textViewOnLines, textViewAppend,
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
    EditText, newTextCursor, copyTextCursor,
    FoldMapLines, MapLines, EditLine, editLine,
    FoldMapLinesHalt,
    FoldMapChars, foldMapChars, runFoldMapChars, execFoldMapChars, evalFoldMapChars,
    MapChars, runMapChars,
    -- * Text Editor Data Structures
    -- ** Text Buffer
    TextBufferState, LineBreaker(..),
    bufferLineBreaker, lineBreaker, lineBreakPredicate, defaultLineBreak,
    bufferCurrentLine, textLineString, bufferDefaultTags,
    lineBreakerNLCR,
    -- ** Line Editing
    TextLine, emptyTextLine, nullTextLine, sliceLineToEnd, textLineIsUndefined,
    TextCursor, newCursorFromLine, textCursorCharCount, textLineTags,
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
import           Control.Monad.State
import           Control.Monad.State.Class

import           Data.Semigroup
import qualified Data.Vector                 as Vec
import qualified Data.Vector.Mutable         as MVec
import qualified Data.Vector.Unboxed         as UVec
import qualified Data.Vector.Generic.Mutable as GMVec
import qualified Data.Vector.Unboxed.Mutable as UMVec
import           Data.Word

import Debug.Trace

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
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Num)

-- | Used for indexing absolute lines and characters (relative to the start of the document, which
-- is line 1).
newtype Absolute a = Absolute a
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Num)

-- | A number for indexing a line. This data type instantiates the 'Prelude.Num' typeclass so that
-- you can write an integer literal in your code and (if used in the correct context) the type
-- inference will automatically declare a 'LineIndex' without you needing to write @(LineIndex 1)@
-- constructor unless you really want to.
newtype LineIndex = LineIndex Int
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Num)

-- | A number for indexing a column, i.e. a character within a line. This data type instantiates
-- the 'Prelude.Num' typeclass so that you can write an integer literal in your code and (if used in
-- the correct context) the type inference will automatically declare a 'CharIndex' without you
-- needing to write @(LineIndex 1)@ constructor unless you really want to.
newtype CharIndex = CharIndex Int
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Num)

----------------------------------------------------------------------------------------------------

-- | This is a type of functions that can modify the textual content stored in a 'TextBufferState'.
newtype EditText tags m a
  = EditText{ unwrapEditText :: ExceptT TextEditError (StateT (TextBufferState tags) m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => MonadState (TextBufferState tags) (EditText tags m) where
  state = EditText . lift . state

instance Monad m => MonadError TextEditError (EditText tags m) where
  throwError = EditText . throwError
  catchError (EditText try) catch = EditText $ catchError try $ unwrapEditText . catch

instance MonadTrans (EditText tags) where
  lift = EditText . lift . lift

-- | Evaluate an 'EditText' function on the given 'TextBufferState'. The given 'EditText' function
-- is evaluates all updates on the given 'TextBuffer' atomically and in-place (i.e. without copying
-- anything unless instructed to do so). This is the most efficient way to evaluate an 'EditText'
-- function, but is more restrictive in that it can only be evaluated when the 'EditText' data
-- type's @m@ parameter is set to the @IO@ type, meaning you cannot use this function if the
-- 'EditText's @m@ parameter is something other than @IO@, like for example a 'ReaderT' type.
runEditTextIO :: EditText tags IO a -> TextBuffer tags -> IO (Either TextEditError a)
runEditTextIO (EditText f) (TextBuffer mvar) = modifyMVar mvar $
  fmap (\ (a,b) -> (b,a)) . runStateT (runExceptT f)

-- | Evaluate an 'EditText' function on the given 'TextBufferState', but unlike 'runEditTextIO', a
-- copy of the entire text buffer is first created, and all updates are performed on the copy
-- atomically. This function is less restrictive than 'runEditTextIO' because it will work for
-- 'EditText' functions where the @m@ parameter is not just @IO@ but any member of the 'Monad'
-- typeclass, however the trade-off is that a copy of the text buffer must be made.
runEditTextOnCopy
  :: MonadIO m
  => EditText tags m a -> TextBuffer tags -> m (Either TextEditError a, TextBuffer tags)
runEditTextOnCopy (EditText f) = liftIO . copyTextBuffer >=> \ copy@(TextBuffer mvar) -> do
  (result, st) <- liftIO (readMVar mvar) >>= runStateT (runExceptT f)
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
-- 'bufferCurrentLine'.
newtype EditLine tags m a
  = EditLine
    { unwrapEditLine :: ExceptT TextEditError (StateT (TextCursor tags) (EditText tags m)) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => MonadState (TextCursor tags) (EditLine tags m) where
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
editLine f = beginInsertMode >>= flip resumeEditLine f

-- not for export
--
-- This function assumes the 'bufferCurrentLine' is set to the current line already, and thus does
-- not evaluate 'beginInsertMode'. It is necessary for all functions in this module to use
-- 'resumeEditLine' rather than 'liftEditLine' internally to prevent an infinite recursions caused
-- by the fact that the exported, public-facing 'editLine' calls 'beginInsertMode'.
resumeEditLine
  :: (MonadIO m
     , Show tags --DEBUG
     ) => Bool -> EditLine tags m a -> EditText tags m a
resumeEditLine modeChanged (EditLine f) = do
  let end = when modeChanged $ void endInsertMode
  use bufferCurrentLine >>= runStateT (runExceptT f) >>= \ case
    (Left err, _   ) -> end >> throwError err
    (Right  a, line) -> bufferCurrentLine .= line >> end >> return a

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
-- 'bufferDefaultTags', 'bufferLineBreaker', and 'bufferCurrentLine', do allow you to modify the
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
      -- string is transferred from 'theBufferCursor' to to 'theLinesAbove' or 'theLinesBelow'.
    , theBufferVector      :: !(MVec.IOVector (TextLine tags))
      -- ^ A mutable vector containing each line of editable text.
    , theLinesAboveCursor  :: !Int
      -- ^ The number of lines above the cursor
    , theLinesBelowCursor  :: !Int
      -- ^ The number of line below the cursor
    , theBufferCursor      :: !(TextCursor tags)
      -- ^ A data structure for editing individual characters in a line of text.
    , theBufferInsertMode  :: !Bool
      -- ^ Whether or not the buffer is in insert mode, which means a 'TextLine' has been copied
      -- into 'theBufferCursor'.
    }

data TextEditError
  = TextEditError       !StrictBytes
  | EndOfLineBuffer     !RelativeToCursor
  | EndOfCharBuffer     !RelativeToCursor
  | LineIndexOutOfRange !(Absolute LineIndex)
  | LineCountOutOfRange !(Relative LineIndex)
  | CharIndexOutOfRange !(Absolute CharIndex)
  | CharCountOutOfRange !(Relative CharIndex)
  deriving (Eq, Ord)

-- | A pair of functions used to break strings into lines. This function is called every time a
-- string is transferred from 'theBufferCursor' to to 'theLinesAbove' or 'theLinesBelow' to ensure
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
      -- ^ This function is called by 'insertChar' to determine if the 'bufferCurrentLine' should be
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

----------------------------------------------------------------------------------------------------

-- | The current line that is being edited.
data TextCursor tags
  = TextCursor
    { theLineEditBuffer    :: !(UMVec.IOVector Char)
    , theCharsBeforeCursor :: !Int
    , theCharsAfterCursor  :: !Int
    , theCursorBreakSize   :: !Word16
    , theTextCursorTags    :: tags
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

-- Not for export: this buffer is formatted such that characters before the cursror are near index
-- zero, while characters after the cursor are near the final index.
lineEditBuffer :: Lens' (TextCursor tags) (UMVec.IOVector Char)
lineEditBuffer = lens theLineEditBuffer $ \ a b -> a{ theLineEditBuffer = b }

-- Not for export: this gets updated when inserting or deleting characters before the cursor.
charsBeforeCursor :: Lens' (TextCursor tags) Int
charsBeforeCursor = lens theCharsBeforeCursor $ \ a b -> a{ theCharsBeforeCursor = b }

-- Not for export: this gets updated when inserting or deleting characters after the cursor.
charsAfterCursor :: Lens' (TextCursor tags) Int
charsAfterCursor = lens theCharsAfterCursor $ \ a b -> a{ theCharsAfterCursor = b}

-- Not for export: controls line break information within the cursor. If this value is zero, it
-- indicates that no line breaking characters have been entered into the cursor yet. If this value
-- is non-zero, it indicates that the line breaking characters do exist after the cursor at some
-- point, and if additional line breaks are inserted the characters after the cursor need to be
-- split off into a new 'TextLine'.
cursorBreakSize :: Lens' (TextCursor tags) Word16
cursorBreakSize = lens theCursorBreakSize $ \ a b -> a{ theCursorBreakSize = b }

-- | A 'Control.Lens.Lens' to get or set tags for the line currently under the cursor. To use or
-- modify the tags value of the line under the cursor, evaluate one of the functions 'use',
-- 'modifying', @('Control.Lens..=')@, or @('Control.Lens.%=')@ within an 'EditText' function, or
-- any function which instantiates 'MonadEditText'.
textCursorTags :: Lens' (TextCursor tags) tags
textCursorTags = lens theTextCursorTags $ \ a b -> a{ theTextCursorTags = b }

-- | Use this to initialize a new empty 'TextBufferState'. The default 'bufferLineBreaker' is set to
-- 'lineBreakerNLCR'. A 'TextBufferState' always contains one empty line, but a line must have a @tags@
-- tag, so it is necessary to pass an initializing tag value of type @tags@ -- if you need nothing
-- but plain text editing, @tags@ can be unit @()@.
newTextBuffer :: tags -> IO (TextBuffer tags)
newTextBuffer = fmap TextBuffer . (newTextBufferState defaultInitBufferSize >=> newMVar)

-- | Create a deep-copy of a 'TextBuffer'. Everything is copied perfectly, including the cursor
-- position, and the content and state of the cursor.
copyTextBuffer :: TextBuffer tags -> IO (TextBuffer tags)
copyTextBuffer (TextBuffer mvar) = withMVar mvar $ \ old -> do
  newBuf <- copyVec (theBufferVector old) (theLinesAboveCursor old) (theLinesBelowCursor old)
  newCur <- copyTextCursor $ theBufferCursor old
  fmap TextBuffer $ newMVar $ old{ theBufferCursor = newCur, theBufferVector = newBuf }

newTextBufferState :: Int -> tags -> IO (TextBufferState tags)
newTextBufferState size tags = do
  cur <- newTextCursor tags
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
    , theLinesAboveCursor  = 0
    , theLinesBelowCursor  = 0
    , theBufferCursor      = cur
    , theBufferInsertMode  = False
    }

-- | Use this to initialize a new empty 'TextCursor'. This is usually only handy if you want to
-- define and test your own 'EditLine' functions and need to evaluate 'editLine' by hand rather than
-- allowing the 'TextEdit' APIs automatically manage line editing. A 'TextBufferState' always
-- contains one empty line, but a line must have a @tags@ tag, so it is necessary to pass an
-- initializing tag value of type @tags@.
newTextCursor :: tags -> IO (TextCursor tags)
newTextCursor tag = do
  buf <- UMVec.new 1024
  return TextCursor
    { theLineEditBuffer    = buf
    , theCharsBeforeCursor = 0
    , theCharsAfterCursor  = 0
    , theCursorBreakSize   = 0
    , theTextCursorTags    = tag
    }

-- | Use this to create a deep-copy of a 'TextCursor'. The cursor position within the 'TextCursor'
-- is also copied.
copyTextCursor :: TextCursor tags -> IO (TextCursor tags)
copyTextCursor cur = do
  buf <- copyVec (theLineEditBuffer cur) (theCharsBeforeCursor cur) (theCharsAfterCursor cur)
  return cur{ theLineEditBuffer = buf }

-- | Determine how many characters have been stored into this buffer.
textCursorCharCount :: TextCursor tags -> Int
textCursorCharCount cur = theCharsBeforeCursor cur + theCharsAfterCursor cur

-- Not for export: unsafe because it does not check for line breaks in the given string. This
-- function copies the characters from a 'TextCursor' buffer into a pure 'TextLine'.
unsafeMakeLine :: TextCursor tags -> IO (TextLine tags)
unsafeMakeLine cur = do
  let buf = theLineEditBuffer cur
  let len = UMVec.length buf
  let chars start count =
        if count <= 0 then pure "" else forM [start .. start + count - 1] $ UMVec.read buf
  let beforeCount = cur ^. charsBeforeCursor
  let afterCount  = cur ^. charsAfterCursor
  beforeChars <- chars 0 beforeCount 
  afterChars  <- chars (len - 1 - afterCount) afterCount
  return TextLine
    { theTextLineString    = packSize (textCursorCharCount cur) $ beforeChars ++ afterChars
    , theTextLineTags      = theTextCursorTags cur
    , theTextLineBreakSize = 0
    }

-- | Create a new 'TextCursor' from a 'TextLine'. The 'TextCursor' can be updated with an 'EditLine'
-- function. Note that this function works in any monadic function type @m@ which instantiates
-- 'Control.Monad.IO.Class.MonadIO', so this will work in the @IO@ monad, the 'EditText' monad, the
-- 'EditLine' monad, and in other contexts as well.
newCursorFromLine :: MonadIO m => Int -> TextLine tags -> m (TextCursor tags)
newCursorFromLine cur line = liftIO $ do
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
  return TextCursor
    { theLineEditBuffer = buf
    , theCharsBeforeCursor = lenBefore
    , theCharsAfterCursor  = lenAfter
    , theCursorBreakSize   = breakSize
    , theTextCursorTags    = theTextLineTags line
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
-- transferred from 'theBufferCursor' to 'theLinesAboveCursor' or 'theLinesBelow'. Note that setting
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

-- | This function is called by 'insertChar' to determine if the 'bufferCurrentLine' should be
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
bufferCurrentLine :: Lens' (TextBufferState tags) (TextCursor tags)
bufferCurrentLine = lens theBufferCursor $ \ a b -> a{ theBufferCursor = b }

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

class MonadIO m => Editor vec m | m -> vec where
  nullElem  :: vec ~ v elem => m elem
  newVector :: Int -> m vec
  modVector :: (vec -> vec) -> m vec
  modBefore :: (Int -> Int) -> m Int
  modAfter  :: (Int -> Int) -> m Int
  throwLimitErr :: RelativeToCursor -> m void
  throwIndexErr :: Int -> m void
  throwCountErr :: Int -> m void

instance MonadIO m => Editor (MVec.IOVector (TextLine tags)) (EditText tags m) where
  nullElem = pure TextLineUndefined
  newVector siz = nullElem >>= liftIO . MVec.replicate siz
  modVector f = state $ \ st -> let vec = f $ theBufferVector st in
    (vec, st{ theBufferVector = vec })
  modBefore f = state $ \ st -> let i = f $ theLinesAboveCursor st in
    (i, st{ theLinesAboveCursor = i})
  modAfter  f = state $ \ st -> let i = f $ theLinesBelowCursor st in
    (i, st{ theLinesBelowCursor = i })
  throwLimitErr = throwError . EndOfLineBuffer
  throwIndexErr = throwError . LineIndexOutOfRange . Absolute . LineIndex
  throwCountErr = throwError . LineCountOutOfRange . Relative . LineIndex

instance MonadIO m => Editor (UMVec.IOVector Char) (EditLine tags m) where
  nullElem = pure '\0'
  newVector siz = nullElem >>= liftIO . UMVec.replicate siz
  modVector f = state $ \ st -> let vec = f $ theLineEditBuffer st in
    (vec, st{ theLineEditBuffer = vec })
  modBefore f = state $ \ st -> let i = f $ theCharsBeforeCursor st in
    (i, st{ theCharsBeforeCursor = i })
  modAfter  f = state $ \ st -> let i = f $ theCharsAfterCursor st in
    (i, st{ theCharsAfterCursor = i })
  throwLimitErr = throwError . EndOfCharBuffer
  throwIndexErr = throwError . CharIndexOutOfRange . Absolute . CharIndex
  throwCountErr = throwError . CharCountOutOfRange . Relative . CharIndex

-- Convenient zero value
m0 :: Monad m => m Int
m0 = return 0

-- The vector of the text buffer.
getVector :: Editor vec m => m vec
getVector = modVector id

-- Returns the number of valid elements on or before the cursor.
countBeforeCur :: Editor vec m => m Int
countBeforeCur = modBefore id

-- The number of valid lines after the cursor.
countAfterCur :: Editor vec m => m Int
countAfterCur = modAfter id

-- The size of the buffer allocation
getAllocSize :: (Editor (vec st elem) m, GMVec.MVector vec elem) => m Int
getAllocSize = GMVec.length <$> getVector

-- The number of valid lines in the buffer @(bufCurIndex + bufferLinesBelowCursor)@.
countLines :: Editor vec m => m Int
countLines = (+) <$> countBeforeCur <*> countAfterCur

-- The number of lines in the buffer that are not valid, @(bufAllocSize - bufLineCount)@
getUnusedSpace :: (Editor (vec st elem) m, GMVec.MVector vec elem) => m Int
getUnusedSpace = subtract <$> countLines <*> getAllocSize

-- The top-most index, regardless of whether the index contains a valid line.
getTopIndex :: (Editor (vec st elem) m, GMVec.MVector vec elem) => m Int
getTopIndex = subtract 1 <$> getAllocSize

-- The index of the top-most valid line, which may not exist. Bounds checking is performed.
getTopIndexChk :: (Editor (vec st elem) m, GMVec.MVector vec elem) => m Int
getTopIndexChk = countAfterCur >>= \ i ->
  if i <= 0 then throwLimitErr After else getTopIndex

-- Translate a 'Before' or 'After' value to an 'Int' index value into a vector. When selecting the
-- element 'After', a bounds check is performed to ensure that the cursor is not at the end of the
-- buffer.
getRelIndex :: (Editor (vec RealWorld elem) m, GMVec.MVector vec elem) => RelativeToCursor -> m Int
getRelIndex = \ case { Before -> countBeforeCur; After -> getTopIndexChk; }

-- Gets the index of the first valid line after the cursor. Bounds checking is __NOT__ performed.
getIndexAfterCur :: (Editor (vec st elem) m, GMVec.MVector vec elem) => m Int
getIndexAfterCur = subtract <$> countAfterCur <*> getAllocSize

-- Gets the index of the first valid line after the cursor. Bounds checking is performed.
getIndexAfterCurChk :: (Editor (vec st elem) m, GMVec.MVector vec elem) => m Int
getIndexAfterCurChk = countAfterCur >>= \ i ->
  if i <= 0 then throwLimitErr After else getIndexAfterCur

-- Get the index within the vector that is associated with the given 'LineIndex'. Bounds checking is
-- performed.
getAbsoluteChk :: (Editor (vec st elem) m, GMVec.MVector vec elem) => Absolute Int -> m Int
getAbsoluteChk (Absolute i) = getAllocSize >>= \ siz ->
  (if not $ 0 <= i && i < siz then throwIndexErr else getAbsolute . Absolute) i

-- Get the index within the vector that is associated with the given 'LineIndex'. Bounds checking is
-- __NOT__ performed.
getAbsolute :: (Editor (vec st elem) m, GMVec.MVector vec elem) => Absolute Int -> m Int
getAbsolute (Absolute i) = countBeforeCur >>= \ cur ->
  if i <= cur then return i else (+ i) <$> getUnusedSpace

-- Convert a @('Relative' 'LineIndex')@ to an @('Absolute' 'LineIndex')@.
getRelToAbs
  :: (Editor (vec st elem) m, GMVec.MVector vec elem)
  => (Relative Int) -> m (Absolute Int)
getRelToAbs (Relative i) = Absolute . (+ i) <$> countBeforeCur

-- Return the starting and ending index of the region of undefined elements.
getVoid
  :: (Editor (vec st elem) m, GMVec.MVector vec elem)
  => m (Maybe (Absolute Int, Absolute Int))
getVoid = do
  rgn <- (,) <$> (Absolute . (+ 1) <$> countBeforeCur)
             <*> (Absolute . subtract 1 <$> countAfterCur)
  return $ guard (uncurry (<=) rgn) >> Just rgn

-- True if there are no undefined text line elements in the buffer.
getIsFull
  :: (Editor (vec st elem) m, GMVec.MVector vec elem)
  => m Bool
getIsFull = (== 0) <$> getUnusedSpace

-- Make a slice of elements relative to the current cursor. A negative argument will take elements
-- before and up-to the cursor, a positive argument will take that many elements starting from
-- 'getLineAfterCur'. Pass a boolean value indicating whether or not you would like to perform
-- bounds checking, if so an exception will be raised if the line index goes out of bounds.
getSlice
  :: (Editor (vec st elem) m, GMVec.MVector vec elem)
  => Bool -> Relative Int -> m (vec st elem)
getSlice doCheck (Relative count) = do
  vec <- getVector
  if count < 0
   then do
    i <- (+ count) <$> countBeforeCur
    if doCheck && i < 0 then throwCountErr count else return $ GMVec.slice i (abs count) vec
   else if count > 0
   then do
    i   <- (+ count) <$> getIndexAfterCur
    top <- getTopIndex
    if doCheck && i > top then throwCountErr count else return $ GMVec.slice i count vec
   else return $ GMVec.slice 0 0 vec

-- Make a slice within the contiguous region of invalid elements after the cursor. This can be used
-- as the target of a vector copy. If the buffer is full, 'Nothing' is returned.
getVoidSlice
  :: (Editor (vec st elem) m, GMVec.MVector vec elem)
  => Relative Int -> m (Maybe (vec st elem))
getVoidSlice (Relative count) = do
  vec <- getVector
  vsp <- getVoid
  return $ vsp <&> \ (Absolute lo, Absolute hi) ->
   if count < 0
   then GMVec.slice (hi + count) (abs count) vec
   else if count > 0
   then GMVec.slice lo count vec
   else GMVec.slice 0 0 vec

-- Obtain a slice (using 'getSlice') for the portion of the vector containing elements before or on
-- the current cursor.
getLoSlice :: (Editor (vec st elem) m, GMVec.MVector vec elem) => m (vec st elem)
getLoSlice = countBeforeCur >>= getSlice False . Relative . negate

-- Obtain a slice (using 'getSliceM') for the portion of the vector containing elements after the
-- current cursor.
getHiSlice :: (Editor (vec st elem) m, GMVec.MVector vec elem) => m (vec st elem)
getHiSlice = countAfterCur >>= getSlice False . Relative

-- Copy the element that is 'Before' (currently on), or the element that is 'After', the current
-- cursor. When selecting the element 'After', a bounds check is performed to ensure that the cursor
-- is not at the end of the buffer.
getCopy1 :: (Editor (vec RealWorld elem) m, GMVec.MVector vec elem) => RelativeToCursor -> m elem
getCopy1 rel = GMVec.read <$> getVector <*> getRelIndex rel >>= liftIO

-- Delete a single element that is 'Before' (currently on), or the element that is 'After', the
-- cursor. When selecting the element 'After', a bounds check is performed to ensure that the cursor
-- is not at the end of the buffer.
getDel1
  :: (Editor (vec RealWorld elem) m, GMVec.MVector vec elem)
  => RelativeToCursor -> m ()
getDel1 rel = do
  GMVec.write <$> getVector <*> getRelIndex rel <*> nullElem >>= liftIO
  void $ modBefore $ subtract 1

-- If, and only if the vector is full, allocate a new vector with double the space of the current
-- vector, copy the elements from the current vector to the new vector, and then replace the current
-- vector with the new one.
growVector
  :: (Editor (vec RealWorld elem) m, GMVec.MVector vec elem)
  => m ()
growVector = do
  siz <- getAllocSize
  cur <- countBeforeCur
  aft <- countAfterCur
  if cur + aft < siz then return () else do
    oldbef <- getLoSlice
    oldaft <- getHiSlice
    newVector (2 * siz) >>= modVector . const
    newbef <- getLoSlice
    newaft <- getHiSlice
    let copy to = liftIO . if unsafeMode then GMVec.unsafeCopy to else GMVec.copy to
    copy newbef oldbef
    copy newaft oldaft

-- Write an element to a vector index, overwriting whatever was there before. __WARNING__: there is
-- no bounds checking.
putElem
  :: (Editor (vec RealWorld elem) m, GMVec.MVector vec elem)
  => Int -> elem -> m ()
putElem i elem = write <$> getVector <*> pure i <*> pure elem >>= liftIO where
  write = if unsafeMode then GMVec.unsafeWrite else GMVec.write

pushElem
  :: (Editor (vec RealWorld elem) m, GMVec.MVector vec elem)
  => elem -> m ()
pushElem elem = do
  growVector
  join $ putElem <$> countBeforeCur <*> pure elem
  void $ modBefore (+ 1)

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
-- used to access 'TextLine's within the 'TextCursor'.
relativeToLine :: RelativeToCursor -> Lens' (TextBufferState tags) Int
relativeToLine = \ case { Before -> linesAboveCursor; After -> linesBelowCursor; }
  -- This function may dissapear if I decide to buffer lines in a mutable vector.

-- Not for export: This function takes a 'RelativeToCursor' value and constructs a lens that can be
-- used to access a character index within the 'bufferCurrentLine'.
relativeToChar :: RelativeToCursor -> Lens' (TextBufferState tags) Int
relativeToChar =
  (bufferCurrentLine .) . \ case { Before -> charsBeforeCursor; After -> charsAfterCursor; }

-- Get an index into the 'bufferVector' from the current position of the cursor.
cursorIndex :: Monad m => RelativeToCursor -> EditText tags m Int
cursorIndex = \ case
  Before -> use linesAboveCursor
  After  -> (-) <$> (MVec.length <$> use bufferVector) <*> use linesBelowCursor

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
    let copy  = if unsafeMode then GMVec.copy  else GMVec.unsafeCopy
    let slice = if unsafeMode then GMVec.slice else GMVec.unsafeSlice
    newVec <- GMVec.new newSize
    copy (slice 0 before newVec) (slice 0 before vec)
    copy (slice (len - after) after newVec) (slice (len - after) after vec)
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
  let copy  = if unsafeMode then GMVec.unsafeCopy  else GMVec.copy
  let slice = if unsafeMode then GMVec.unsafeSlice else GMVec.slice
  newVec <- GMVec.new len
  when (before > 0) $ copy (slice     0 before newVec) (slice     0 before oldVec)
  when (after  > 0) $ copy (slice upper  after newVec) (slice upper  after oldVec)
  return newVec

-- Not for export: should be executed automatically by insertion operations. Increases the size of
-- the 'lineEditBuffer' to be large enough to contain the current 'textCursorCharCount' plus the
-- given number of elements.
growLineIfTooSmall :: MonadIO m => Int -> EditText tags m ()
growLineIfTooSmall grow = do
  cur <- use bufferCurrentLine
  buf <- liftIO $
    growVec (cur ^. lineEditBuffer) (cur ^. charsBeforeCursor) (cur ^. charsAfterCursor) grow
  bufferCurrentLine . lineEditBuffer .= buf

growBufferIfTooSmall :: MonadIO m => Int -> EditText tags m ()
growBufferIfTooSmall grow = do
  buf   <- use bufferVector
  above <- use linesAboveCursor
  below <- use linesBelowCursor
  liftIO (growVec buf above below grow) >>= assign bufferVector

-- | Push a 'TextLine' before or after the cursor. This function does not effect the content of the
-- 'bufferCurrentLine'.
pushLine
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags -- DEBUG
     )
  => RelativeToCursor -> TextLine tags -> editor tags m ()
pushLine rel line = liftEditText $ do
  growBufferIfTooSmall 1
  i <- cursorIndex rel --DEBUG
  traceM $ "pushLine: rel="++show rel++", cursorIndex="++show i++", line="++show line --DEBUG
  MVec.write <$> use bufferVector <*> cursorIndex rel <*> pure line >>= liftIO
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
    liftIO (MVec.read vec i <* MVec.write vec i TextLineUndefined) <*
      ((case rel of{ Before -> linesAboveCursor; After -> linesBelowCursor; }) -= 1)
  traceM $ "unsafePopLine: "++show line --DEBUG
  return line --DEBUG

-- | Pop a 'TextLine' from before or after the cursor. This function does not effect the content of
-- the 'bufferCurrentLine'. If you 'popLine' from 'Before' the cursor when the 'bufferCurrentLine'
-- is at the beginning of the buffer, or if you 'popLine' from 'After' the cursor when the
-- 'bufferCurrentLine' is at the end of the buffer, 'Prelude.Nothing' will be returned.
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

-- | Get the current line number of the cursor.
currentLineNumber
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m (Absolute LineIndex)
currentLineNumber = liftEditText $
  Absolute . LineIndex . (+ 1) <$> use linesAboveCursor

-- | Get the current column number of the cursor.
currentColumnNumber
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m (Absolute CharIndex)
currentColumnNumber = liftEditText $
  Absolute . CharIndex . (+ 1) . theCharsBeforeCursor <$> use bufferCurrentLine

-- | Get the current cursor position.
currentTextLocation
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m TextLocation
currentTextLocation = TextLocation <$> currentLineNumber <*> currentColumnNumber

-- | Create a copy of the 'bufferCurrentLine'.
copyCurrentLine
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m (TextLine tags)
copyCurrentLine = liftEditText $ use bufferCurrentLine >>= liftIO . unsafeMakeLine

-- | Read a 'TextLine' from an @('Absolute' 'LineIndex')@ address. If the index is out of bounds,
-- 'Prelude.Nothing' is returned.
readLineIndex
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Absolute LineIndex -> editor tags m (Maybe (TextLine tags))
readLineIndex (Absolute (LineIndex i')) = liftEditText $ let i = i' - 1 in
  if i < 0 then return Nothing else do
    above <- use linesAboveCursor
    vec   <- use bufferVector
    let len = MVec.length vec
    if i < above then liftIO $ Just <$> MVec.read vec i else do
      below <- use linesBelowCursor
      i <- pure $ i - above
      if i >= below then return Nothing else liftIO $ Just <$> MVec.read vec (len - below + i - 1)

-- | Write a 'TextLine' (as produced by 'copyCurrentLine' or readLineIndex') to an @('Absolute'
-- 'LineIndex')@ address. If the index is out of bounds, 'Prelude.False' is returned.
writeLineIndex
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Absolute LineIndex -> TextLine tags -> editor tags m Bool
writeLineIndex (Absolute (LineIndex i')) line = liftEditText $ let i = i' - 1 in
  if i < 0 then return False else do
    above <- use linesAboveCursor
    vec   <- use bufferVector
    let len = MVec.length vec
    if i <= above
     then do
      liftIO $ MVec.write vec i line
      when (i == above) $ linesAboveCursor += 1
      return True
     else do
      below <- use linesBelowCursor
      i     <- pure $ i - above
      if i >= below
       then if i /= below then return False else do
        liftIO $ MVec.write vec (len - below - 1) line
        linesBelowCursor += 1
        return True
       else liftIO (MVec.write vec (len - below + i - 1) line) >> return True

-- | If not already in 'bufferInsertMode', this function copies the 'TextLine' at the 'LineIndex'
-- given by 'linesAboveCursor' (i.e. it copies the line under the cursor) into the local
-- 'bufferCurrentLine' line editor. Functions that evaluate to an 'EditLine' function type can then
-- perform a character-by-character edit on the 'bufferCurrentLine' line editor. No 'TextLine's in
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
beginInsertMode = liftEditText $ do
  insMode <- use bufferInsertMode
  if insMode then return False else do
    cur   <- Absolute . CharIndex . subtract 1 <$> use (bufferCurrentLine . charsBeforeCursor)
    vec   <- use bufferVector
    above <- use linesAboveCursor
    traceM $ "beginInsertMode: cur="++show cur++", above="++show above
    -- Set 'bufferInsertMode' before calling 'replaceCurrentLine' to avoid a possible accidental
    -- infinite recursion.
    bufferInsertMode .= True
    liftIO (MVec.read vec above) >>= resumeEditLine False . editReplaceCurrentLine cur
    return True

-- | If in 'bufferInsertMode', this function places the 'bufferCurrentLine' back into the text
-- buffer, overwriting the 'TextLine' at the 'LineIndex' given by 'linesAboveCursor'. This function
-- then clears the 'bufferCurrentLine' line editor.
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
endInsertMode = liftEditText $ do
  insMode <- use bufferInsertMode
  if not insMode then return False else do
    vec   <- use bufferVector
    above <- use linesAboveCursor
    (copyCurrentLine <* clearCurrentLine) >>= liftIO . MVec.write vec (above - 1)
    bufferInsertMode .= False
    return True

-- | Replace the content in the 'bufferCurrentLine' with the content in the given 'TextLine'. Pass
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
editReplaceCurrentLine (Absolute (CharIndex cur)) line = do
  let srcvec = line ^. textLineString
  let srclen = intSize srcvec
  cur <- pure $ max 0 $ min (cur - 1) srclen
  targvec <- use lineEditBuffer
  let targlen = UMVec.length targvec
  targvec <- if targlen >= srclen then return targvec else liftIO $
    UMVec.new $ head $ dropWhile (< srclen) $ iterate (* 2) targlen
  let targlen = UMVec.length targvec
  charsBeforeCursor .= cur
  charsAfterCursor  .= srclen - cur
  textCursorTags    .= line ^. textLineTags
  liftIO $ do
    let copy = mapM_ $ UMVec.write targvec *** (srcvec UVec.!) >>> app
    copy $ (id &&& id) <$> [0 .. cur - 1]
    copy $ zip [targlen - cur - 1 .. targlen - 1] [srclen - cur - 1 .. srclen - 1]

-- | Delete the content of the 'bufferCurrentLine' except for the line breaking characters (if any)
-- at the end of the line. This function does not change the memory allocation for the 'TextCursor',
-- it simply sets the character count to zero. Tags on this line are not effected.
clearCurrentLine
  :: (MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m ()
clearCurrentLine = liftEditLine $ do
  charsBeforeCursor .= 0
  use cursorBreakSize >>= assign charsAfterCursor . fromIntegral

-- | Like 'clearCurrentLine', this function deletes the content of the 'bufferCurrentLine', and tags
-- on this line are not effected. The difference is that this function replaces the
-- 'bufferCurrentLine' with a new empty line, resetting the line editor buffer to the default
-- allocation size and allowing the garbage collector to delete the previous allocation. This means
-- the line editor buffer memory allocation may be shrunk to it's minimal/default size.
resetCurrentLine
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m ()
resetCurrentLine = liftEditText $
  use (bufferCurrentLine . textCursorTags) >>= liftIO . newTextCursor >>= assign bufferCurrentLine

-- Not for export: exposes structure of internal mutable vector. Moves a region of elements near the
-- cursor from top to bottom, or from bottom to top. Negative value for select indicates to select
-- elements before the cursor, positive means after the cursor. Returns updated before and after
-- values.
shiftElems
  :: (GMVec.MVector vector a, PrimMonad m)
  => a -> vector (PrimState m) a -> Int -> Int -> Int -> m (Int, Int)
shiftElems nil vec select' before after =
  if select == 0 then noop
  else if select > 0
  then if after  <= 0 then noop else
       if select ==   1  then moveSingle before (len - after) else moveBlock
  else if select < 0
  then if before <= 0 then noop else
       if select == (-1) then moveSingle (len - 1 - after) before else moveBlock
  else error $ "internal error: this should never happen. " ++
         "(shiftElems " ++ show select' ++ ' ':show before ++ ' ':show after ++ ")"
  where
    noop   = return (before, after)
    select = max (negate before) . min after $ select'
    len    = GMVec.length vec
    slice  = if unsafeMode then GMVec.unsafeSlice else GMVec.slice
    (toSlice, fromSlice) =
      if select > 0
      then (slice before select vec, slice (len - after) select vec)
      else if select < 0 then
        ( slice (len - after + select) (negate select) vec
        , slice (before + select) (negate select) vec
        )
      else error $ "shiftElems: select="++show select++", this should never happen"
    clear slice = forM_ [0 .. GMVec.length slice - 1] $ flip (GMVec.write slice) nil
    done = return (before + select, after - select)
    moveSingle to from =
      GMVec.read vec from >>= GMVec.write vec to >> GMVec.write vec from nil >> done
    moveBlock = 
      GMVec.move toSlice fromSlice >> clear fromSlice >> done

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
moveByLine (Relative (LineIndex select)) = liftEditText $ do
  vec <- use bufferVector
  (before, after) <- (,) <$> use linesAboveCursor <*> use linesBelowCursor
  (before, after) <- liftIO $ shiftElems TextLineUndefined vec select before after
  linesAboveCursor .= before
  linesBelowCursor .= after

-- | Move the cursor to a different character position within the 'bufferCurrentLine' by an @n ::
-- Int@ number of characters. A negative @n@ indicates moving toward the start of the line, a
-- positive @n@ indicates moving toward the end of the line.
moveByChar
  :: (MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Relative CharIndex -> editor tags m ()
moveByChar (Relative (CharIndex select)) = liftEditLine $ do
  after  <- use charsAfterCursor
  lbrksz <- use cursorBreakSize
  select <- pure $ if select <= 0 then select else min select $ max 0 (after - fromIntegral lbrksz)
  vec    <- use lineEditBuffer
  (before, after) <- (,) <$> use charsBeforeCursor <*> use charsAfterCursor >>=
    liftIO . uncurry (shiftElems '\0' vec select)
  charsBeforeCursor .= before
  charsAfterCursor  .= after

-- Not for export: does not filter line-breaking characters.
unsafeInsertChar :: MonadIO m => RelativeToCursor -> Char -> EditText tags m ()
unsafeInsertChar rel c = do
  growLineIfTooSmall 1
  buf <- use $ bufferCurrentLine . lineEditBuffer
  off <- use $ relativeToChar rel
  let len = UMVec.length buf
  let i   = case rel of { Before -> off; After -> len - off - 1; }
  relativeToChar rel += 1
  liftIO $ UMVec.write buf i c

-- Not for export: does not filter line-breaking characters.
unsafeInsertString :: MonadIO m => RelativeToCursor -> CharVector -> EditText tags m ()
unsafeInsertString rel str = do
  let strlen = intSize str
  growLineIfTooSmall strlen
  buf <- use $ bufferCurrentLine . lineEditBuffer
  off <- use $ relativeToChar rel
  let len = UMVec.length buf
  let i0  = case rel of { Before -> off; After -> len - off - strlen; }
  relativeToChar rel += strlen
  liftIO $ forM_ (zip [0 ..] $ unpack str) $ \ (i, c) -> UMVec.write buf (i0 + i) c

-- | Insert a single character. If the 'lineBreakPredicate' function evaluates to 'Prelude.True',
-- meaning the given character is a line breaking character, this function does nothing. To insert
-- line breaks, using 'insertString'.
insertChar
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => RelativeToCursor -> Char -> editor tags m ()
insertChar rel c = liftEditText $ do
  isBreak <- use $ bufferLineBreaker . lineBreakPredicate
  if isBreak c then return () else unsafeInsertChar rel c

-- | Go to an absolute line number, the first line is 1 (line 0 and below all send the cursor to
-- line 1), the last line is 'Prelude.maxBound'.
gotoLine
  :: (MonadEditText editor, MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Absolute LineIndex -> editor tags m ()
gotoLine (Absolute (LineIndex n)) = liftEditText $ do
  above <- use linesAboveCursor
  moveByLine $ Relative $ LineIndex $ n - above

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

-- | This function only deletes characters on the current line, if the cursor is at the start of the
-- line and you evaluate @'deleteChars' 'Before'@, this function does nothing. This function does
-- not delete line breaking characters. Returns the number of characters actually deleted.
deleteChars
  :: (MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Relative CharIndex -> editor tags m (Relative CharIndex)
deleteChars (Relative (CharIndex n)) = liftEditLine $ fmap (Relative . CharIndex) $
  if n < 0 then do
    before <- use charsBeforeCursor
    let count = min before $ negate n 
    charsBeforeCursor .= before - count
    return count
  else if n > 0 then do
    lbrksz <- fromIntegral <$> use cursorBreakSize
    after  <- use charsAfterCursor
    let count = min n $ max 0 $ after - lbrksz
    charsAfterCursor .= after - count
    return count
  else return 0

-- | This function deletes characters starting from the cursor, and if the number of characters to
-- be deleted exceeds the number of characters in the current line, characters are deleted from
-- adjacent lines such that the travel of deletion wraps to the end of the prior line or the
-- beginning of the next line, depending on the direction of travel.
deleteCharsWrap
  :: forall editor tags m . (MonadEditText editor, MonadIO (editor tags m), MonadIO m
                            , Show tags --DEBUG
                            )
  => Relative CharIndex -> editor tags m (Relative CharIndex)
deleteCharsWrap rem@(Relative (CharIndex remaining)) = liftEditText $ do
  insMode <- use bufferInsertMode
  if remaining < 0 then
    if insMode then do
      (Relative (CharIndex delCount)) <- deleteChars rem
      remaining <- pure $ remaining - delCount
      popLine Before >>= \ case
        Nothing -> return $ Relative $ CharIndex delCount
        Just{}  -> fmap (Relative . CharIndex . fst) $
          forLines Before (delCount, remaining) $ \ halt -> \ case
            TextLineUndefined -> error "deleteCharsWrap: forLines iterated on undefined line"
            line              -> get >>= \ result@(delCount, remaining) ->
              if remaining <= 0 then halt result else do
                let lbrksz = fromIntegral $ theTextLineBreakSize line
                let len    = intSize line - min 1 lbrksz
                put (delCount + len, remaining - len)
                if len < remaining then return [] else do
                  liftEditText $ unsafeInsertString After $ theTextLineString $
                    sliceLineToEnd After (Absolute $ CharIndex $ len - remaining) line
                  pure <$> copyCurrentLine
    else do
      firstLine <- popLine Before
      case firstLine of
        Nothing                     -> return $ Relative $ CharIndex 0
        Just (firstLine@TextLine{}) -> do
          charCur <- use $ bufferCurrentLine . charsBeforeCursor
          fmap (Relative . CharIndex . fst) $ forLines After (0, remaining) $ \ halt -> \ case
            TextLineUndefined -> error "deleteCharsWrap: forLines iterated on undefined line"
            line              -> get >>= \ result@(delCount, remaining) ->
              if remaining <= 0 then halt result else do
                let lbrksz = fromIntegral $ theTextLineBreakSize line
                let len    = intSize line - min 1 lbrksz
                put (delCount + len, remaining - len)
                return $ if len < remaining then [] else pure TextLine
                  { theTextLineString    =
                      UVec.slice 0 (intSize line - lbrksz - remaining) (theTextLineString line) <>
                      UVec.slice charCur (intSize firstLine - charCur) (theTextLineString firstLine)
                  , theTextLineTags      = theTextLineTags line
                  , theTextLineBreakSize = theTextLineBreakSize firstLine
                  }
        _                           -> error $
          "deleteCharsWrap: unsafePopLine Before --> TextLineUndefined"++
          "\n  remaining = "++show remaining++"\n"
  else if remaining > 0 then do
    error "TODO: deleteCharsWrap After"
  else return $ Relative $ CharIndex 0

-- | This function evaluates the 'lineBreaker' function on the given string, and beginning from the
-- current cursor position, begins inserting all the lines of text produced by the 'lineBreaker'
-- function.
insertString
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => String -> editor tags m ()
insertString str = liftEditText $ use (bufferLineBreaker . lineBreaker) >>= init . ($ str) where
  writeStr dbginput =
    trace ("writeStr "++show dbginput) $ --DEBUG
    mapM_ (unsafeInsertChar Before)
    dbginput --DEBUG
  writeLine dbginput@(str, lbrk) =
    trace ("writeLine "++show dbginput) $ --DEBUG
    do  writeStr str
        writeStr lbrk
        when (not $ null lbrk) $ do
          relativeToChar Before += length lbrk
          cur <- use bufferCurrentLine
          let buf = cur ^. lineEditBuffer
          let beforeCount = cur ^. charsBeforeCursor - 1
          beforeChars <- liftIO $ forM [0 .. beforeCount - 1] $ UMVec.read buf
          pushLine Before $ TextLine
            { theTextLineString    = packSize beforeCount beforeChars
            , theTextLineTags      = cur ^. textCursorTags
            , theTextLineBreakSize = fromIntegral $ length lbrk
            }
          trace ("clearCurrentLine") $ clearCurrentLine
  init = \ case
    []                 -> return ()
    [(str,"")]         -> writeStr str
    line@(_,lbrk):more -> do
      join $ editLine $ do
        vec  <- use lineEditBuffer
        cur  <- use charsAfterCursor
        tags <- use textCursorTags
        let len = UMVec.length vec
        if cur <= 0 then return $ pure () else do
          cut <- liftIO $! UVec.freeze $! UMVec.slice (len - cur - 1) cur vec
          --cut <- liftIO $ packSize cur <$> forM [len - cur - 1 .. len - 1] (UMVec.read vec)
          return $ do
            vec <- use bufferVector
            unless (null lbrk) $ linesBelowCursor += 1
            cur <- use linesBelowCursor
            liftIO $ MVec.write vec (MVec.length vec - cur) $ TextLine
              { theTextLineString    = cut
              , theTextLineTags      = tags
              , theTextLineBreakSize = 0
              }
      writeLine line
      loop more
  loop = \ case
    []         -> return ()
    [(str,"")] -> writeStr str
    line:more  -> writeLine line >> loop more

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
-- at the 'bufferCurrentLine', and if the 'RelativeToCursor' value is 'After' then iteration goes
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
      cursor <- use bufferCurrentLine
      let vec = cursor ^. lineEditBuffer
      let cur = cursor ^. charsBeforeCursor
      str <- liftIO $! UVec.freeze $! UMVec.slice 0 cur vec
      pushLine Before $ TextLine
        { theTextLineTags      = cursor ^. textCursorTags
        , theTextLineString    = str
        , theTextLineBreakSize = lbrkSize
        }
      bufferCurrentLine . charsBeforeCursor .= 0
    After  -> do
      cursor <- use bufferCurrentLine
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
        bufferCurrentLine . charsAfterCursor .= 0
      unsafeInsertString After lbrk

----------------------------------------------------------------------------------------------------

data TextLocation
  = TextLocation
    { theLocationLineIndex :: !(Absolute LineIndex)
    , theLocationCharIndex :: !(Absolute CharIndex)
    }
  deriving (Eq, Ord)

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

reorder :: Ord a => a -> a -> (a, a)
reorder a b = (min a b, max a b)

-- | Create a 'TextView' from the content of a 'TextBuffer' in the given range delimited by the two
-- given 'TextLocation' values.
--
-- __NOTE__ that if the range contains the current 'TextCursor', be sure to evaluate 'endInsertMode'
-- function before evaluating 'textViewOnLines', otherwise the updates to the current line will not
-- be reflected in the created 'TextView'.
textView
  :: MonadIO m
  => TextLocation -> TextLocation
  -> TextBuffer tags -> m (TextView tags)
textView from0 to0 (TextBuffer mvar) = liftIO $ withMVar mvar $ \ st -> do
  let (from, to) = reorder from0 to0
  let (Absolute (LineIndex fromLine0)) = theLocationLineIndex from
  let (Absolute (LineIndex toLine0))   = theLocationLineIndex to
  let (Absolute (CharIndex fromChar0)) = theLocationCharIndex from
  let (Absolute (CharIndex toChar0))   = theLocationCharIndex to
  let oldBuf    = theBufferVector     st
  let above     = theLinesAboveCursor st
  let below     = theLinesBelowCursor st
  let lineCount = above + below
  let upper     = MVec.length oldBuf - below
  let limit top = max 0 . min top . subtract 1
  let fromLine  = limit lineCount fromLine0
  let toLine    = limit lineCount toLine0
  let newLen    = toLine - fromLine + 1
  let copy      = if unsafeMode then MVec.unsafeCopy  else MVec.copy
  let slice     = if unsafeMode then MVec.unsafeSlice else MVec.slice
  if lineCount < 0
   then error $ "textView: (lineCount="++show lineCount++") < 0, this should never happen"
   else do
    newBuf <- MVec.new newLen
    if fromLine == toLine
     then MVec.write newBuf 0 =<<
          MVec.read oldBuf (if fromLine < above then fromLine else upper - above + fromLine)
     else if fromLine < above && toLine <  above
     then copy newBuf (slice fromLine newLen oldBuf)
     else if fromLine >= above && toLine >= above
     then copy newBuf (slice (upper - above + fromLine) newLen oldBuf)
     else if fromLine < above && toLine >= above
     then do
       let aboveLen = above - fromLine
       copy (slice 0 aboveLen newBuf) (slice fromLine aboveLen oldBuf)
       let belowLen = toLine - above + 1
       copy (slice aboveLen belowLen newBuf) (slice upper belowLen oldBuf)
     else error "textView: this should never happen"
    let trim idx slice = MVec.read newBuf idx >>= \ case
          TextLineUndefined -> error $ "newBuf["++show idx++"] is undefined"
          line              -> do
            let vec = theTextLineString line
            let len = UVec.length vec
            let (fromChar, toChar) = (limit (len - 1) fromChar0, limit (len - 1) toChar0)
            MVec.write newBuf idx $ line
              { theTextLineString =
                  UVec.force $ uncurry UVec.slice (slice len fromChar toChar) vec
              }
    if fromLine == toLine
      then trim 0 $ \ _len from to -> (min from to, abs (to - from) + 1)
      else do
        trim 0 $ \ len from _to -> (from, len - from)
        trim (newLen - 1) $ \ _len _from to -> (0, to + 1)
    newBuf <- Vec.unsafeFreeze newBuf
    return TextView
      { textViewCharCount = sum $ Vec.toList newBuf >>= \ case
          TextLineUndefined               -> []
          TextLine{theTextLineString=vec} -> [UVec.length vec]
      , textViewVector    = newBuf
      }

-- | Like 'textView', creates a new text view, but rather than taking two 'TextLocation's to delimit
-- the range, takes two @('Absolute' 'LineIndex')@ values to delimit the range.
textViewOnLines
  :: MonadIO m
  => TextLocation -> TextLocation
  -> TextBuffer tags -> m (TextView tags)
textViewOnLines from to = textView
  (TextLocation
   { theLocationLineIndex = min (theLocationLineIndex from) (theLocationLineIndex to)
   , theLocationCharIndex = Absolute $ CharIndex 0
   })
  (TextLocation
   { theLocationLineIndex = max (theLocationLineIndex from) (theLocationLineIndex to)
   , theLocationCharIndex = Absolute $ CharIndex maxBound
   })

-- | Create a new, editable 'TextBuffer' from a read-only 'TextView'. Pass 'Before' to place the
-- text in the buffer before the cursor, meaning the cursor will start positioned at the end of the
-- editable 'TextBuffer', or pass 'After' to place the text in the buffer after the cursor, meaning
-- the cursor will start at the beginning of the editable 'TextBuffer'.
newTextBufferFromView :: MonadIO m => RelativeToCursor -> tags -> TextView tags -> m (TextBuffer tags)
newTextBufferFromView rel tags (TextView{textViewVector=vec}) = liftIO $ do
  let oldLen = Vec.length vec
  st0 <- newTextBufferState (max 16 $ oldLen * 2) tags
  let newBuf = theBufferVector st0
  let newLen = MVec.length newBuf
  let cursor = if oldLen == 0 then 0 else
        if theTextLineBreakSize (vec Vec.! (oldLen - 1)) > 0 then oldLen else oldLen - 1
  let (idx, st) = case rel of
        Before -> ([0 ..], st & linesAboveCursor .~ cursor)
        After  -> ([newLen - oldLen ..], st & linesBelowCursor .~ cursor)
  forM_ (zip idx $ Vec.toList vec) $ uncurry $ MVec.write newBuf
  TextBuffer <$> newMVar st

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
              if nullCount < 4 then putStrLn "...." else return ()
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

-- | The 'bufferCurrentLine', which is a 'TextCursor' is a separate data structure contained within
-- the 'TextBuffer', and it is often not necessary to know this information when debugging, so you
-- can print debugging information about the 'TextCursor' by evaluating this function whenever it is
-- necessary.
debugPrintCursor
  :: (MonadEditText editor, Show tags, MonadIO (editor tags m), MonadIO m)
  => editor tags m ()
debugPrintCursor = liftEditText $ do
  cur <- use bufferCurrentLine
  let charVec    = theLineEditBuffer cur
  let charVecLen = UMVec.length charVec
  let before     = cur ^. charsBeforeCursor
  let after      = cur ^. charsAfterCursor
  liftIO $ do
    str <- forM [0 .. charVecLen - 1] $ UMVec.read charVec
    putStrLn $ "    bufferCurrentLine: " ++ show str
    putStrLn $ "       textCursorTags: " ++ show (cur ^. textCursorTags)
    putStrLn $ " __cursorVectorLength: " ++ show charVecLen
    putStrLn $ "    charsBeforeCursor: " ++ show before
    putStrLn $ "     charsAfterCursor: " ++ show after
    putStrLn $ "      cursorInputSize: " ++ show (before + after)
    putStrLn $ "  cursorLineBreakSize: " ++ show (theCursorBreakSize cur)
