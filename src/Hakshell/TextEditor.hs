-- | This module provides integrated text processing facilities to Hakshell.
--
-- * Why a shell needs a built-in editor
-- 
-- Any useful system shell needs some form of text editor. Some systems, like traditional UNIX
-- systems, prefer to keep the editor separate from the shell's command line interpreter. In Linux,
-- for example, you have Bash as the interpreter, and you can choose from a variety of editor
-- programs depending on your needs: Ed, Sed, Awk, Ex, Vi, Nano, Emacs, and so on. Sed and Awk are
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
-- distinction between a shell and an editor is really not so distinct. Binary data is of course
-- also used throughout the UNIX system, but as often as possible there are tools for representing
-- the binary information as text, and even re-constructing binary from it's textual representation.
--
-- Bash still has extensive commands for manipulating text. Much of this functionality, for example,
-- Globs and Regular Expressions, are in fact a wrapper around the same libraries used by the text
-- editor programs, the only difference is that in Bash these functions are invoked through the
-- various Bash programming language features (often embedded in bash syntax, like when you run the
-- command @ls *@), where as in an editor like Vi or Emacs, these functions are invoked
-- interactively, for example, when doing an interactive text search/replace operation.
--
-- Emacs itself further eliminates the distinction between shell and text editor. It is even
-- arguable as to whether Emacs is a text editor at all. Some people jokingly refer to it as an
-- "operating system that lacks a useful text editor." A more accurate description of Emacs would be
-- a "Lisp interpreter that allows one to easily invent various text editors for various specific
-- use cases." You could also call Emacs an IDE, or an app platform, both of which require text
-- editing facilities. Emacs also has very good shell integration: there are commands that execute
-- shell commands with buffered text as input, and capturing the output text of the process back
-- into a buffer.
--
-- Emacs is, in many ways, can perform all the same functions as Bash, while providing additional
-- interactive feautres. Emacs is therefore a more advanced system shell with integrated text
-- editing features. It is possible, although not commonly done, to use Emacs in place of bash as
-- the login shell process, and anyone logging in to the system would have no difficulty using any
-- and all of the services provided to them by the server.
--
-- The "Hakshell.TextEditor" module is therefore designed to provide a solid, consistent foundation
-- for automated text processing APIs, upon which programmers can build text editors and text
-- processors, and hence it is an integral part of the "Hakshell" library.
module Hakshell.TextEditor
  ( -- * Text Editing API
    MonadEditText(..),
--    insertCharBefore, insertCharAfter, deleteCharsBefore, deleteCharsAfter,
--    insertStringAfter, insertStringBefore, 
    -- * Text Editor Function Types
    EditText, runEditText, emptyTextBuffer, emptyTextCursor,
    FoldMapLines, runFoldMapLines, execFoldMapLines, evalFoldMapLines,
    MapLines, runMapLines,
    EditLine, editLine,
    FoldMapChars, foldMapChars, runFoldMapChars, execFoldMapChars, evalFoldMapChars,
    MapChars, runMapChars,
    -- * Text Editor Data Structures
    -- ** Text Buffer
    TextBuffer, LineBreaker, theBufferCharCount, theBufferCharNumber, theBufferLineCount,
    bufferAboveCursor, bufferLineBreaker, bufferCurrentLine, bufferBelowCursor, textLineString,
    lineBreakNLCR,
    -- ** Line Editing
    TextLine, TextCursor, RelativeToCursor, textCursorBefore, textCursorAfter, textLineTags,
    -- ** Errors
    TextEditError(..),
    -- * Re-exporting "Hakshell.String"
    module Hakshell.String
  ) where

import           Hakshell.String

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State

import qualified Data.ByteString           as Strict
import qualified Data.ByteString.UTF8      as UTF8st
import qualified Data.ByteString.Lazy      as Lazy
import qualified Data.ByteString.Lazy.UTF8 as UTF8lz
import           Data.Sequence

----------------------------------------------------------------------------------------------------

-- | This is a type of functions that can modify the textual content stored in a 'TextBuffer'.
newtype EditText line a
  = EditText{ unwrapEditText :: ExceptT TextEditError (State (TextBuffer line)) a }
  deriving (Functor, Applicative, Monad)

instance MonadState (TextBuffer line) (EditText line) where { state = EditText . lift . state; }

instance MonadError TextEditError (EditText line) where
  throwError = EditText . throwError
  catchError (EditText try) catch = EditText $ catchError try $ unwrapEditText . catch

----------------------------------------------------------------------------------------------------

-- | A type of functions that can perform a fold (in the sense of the Haskell 'Control.Monad.foldM'
-- function) over lines of text in a 'TextBuffer'. This function takes an arbitrary @fold@ data type
-- which can be anything you want, and is initialized when evaluating the 'runFoldMapLines'
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
newtype FoldMapLines fold line a
  = FoldMapLines{ unwrapFoldMapLines :: ExceptT TextEditError (StateT fold (EditText line)) a }
  deriving (Functor, Applicative, Monad)

instance MonadState fold (FoldMapLines fold line) where
  state = FoldMapLines . lift . state

instance MonadError TextEditError (FoldMapLines fold line) where
  throwError = FoldMapLines . throwError
  catchError (FoldMapLines try) catch = FoldMapLines $ catchError try $ unwrapFoldMapLines . catch

-- | Convert a 'FoldMapLines' into an 'EditText' function. This function is analogous to the
-- 'runStateT' function.
runFoldMapLines :: FoldMapLines fold line a -> fold -> EditText line (a, fold)
runFoldMapLines (FoldMapLines f) = runStateT (runExceptT f) >=> \ case
  (Left err, _   ) -> throwError err
  (Right  a, fold) -> return (a, fold)

-- | Like 'runFoldMapLines' but only returns the @fold@ result. This function is analogous to the
-- 'execStateT' function.
execFoldMapLines :: FoldMapLines fold line a -> fold -> EditText line fold
execFoldMapLines = fmap (fmap snd) . runFoldMapLines

-- | Like 'runFoldMapLines' but ignores the @fold@ result. This function is analogous to the
-- 'evalStateT' function.
evalFoldMapLines :: FoldMapLines fold line a -> fold -> EditText line a
evalFoldMapLines = fmap (fmap fst) . runFoldMapLines

----------------------------------------------------------------------------------------------------

-- | A type synonym for a 'FoldMapLines' function in which the folded type is the unit @()@ value.
type MapLines = FoldMapLines ()

-- | Evaluate a 'MapLines' using 'evalFoldMapLines'.
runMapLines :: MapLines line a -> EditText line a
runMapLines = flip evalFoldMapLines ()

----------------------------------------------------------------------------------------------------

-- | Functions of this type operate on a single 'TextLine', it is useful for updating the
-- 'bufferCurrentLine'.
newtype EditLine line a
  = EditLine{ unwrapEditLine :: ExceptT TextEditError (StateT (TextCursor line) (EditText line)) a }
  deriving (Functor, Applicative, Monad)

instance MonadState (TextCursor line) (EditLine line) where
  state = EditLine . lift . state

instance MonadError TextEditError (EditLine line) where
  throwError = EditLine . throwError
  catchError (EditLine try) catch = EditLine $ catchError try $ unwrapEditLine . catch

-- | Perform an edit on the line under the cursor. It is usually not necessary to invoke this
-- function directly, the definition of 'liftEditLine' for the 'TextEdit' function type is this
-- function, so any function that evaluates to an @editor@ where the @editor@ is a member of the
-- 'MonadEditLine' typeclass will automatically invoke this function based on the function type of
-- the context in which it is used.
editLine :: EditLine line a -> EditText line a
editLine (EditLine f) = use bufferCurrentLine >>= runStateT (runExceptT f) >>= \ case
  (Left err, _   ) -> throwError err
  (Right  a, line) -> bufferCurrentLine .= line >> return a

----------------------------------------------------------------------------------------------------

-- | Functions of this type operate on a single 'TextLine', and can perform a fold over characters
-- in the line.
newtype FoldMapChars fold line a
  = FoldMapChars{ unwrapFoldMapChars :: ExceptT TextEditError (StateT fold (EditLine line)) a }
  deriving (Functor, Applicative, Monad)

instance MonadState fold (FoldMapChars fold line) where
  state = FoldMapChars . lift . state

instance MonadError TextEditError (FoldMapChars fold line) where
  throwError = FoldMapChars . throwError
  catchError (FoldMapChars try) catch = FoldMapChars $ catchError try $ unwrapFoldMapChars . catch

-- | Evaluate a 'FoldMapChars' function within a 'FoldMapLines' function, using the same @fold@
-- value from the 'FodlMapLines' state as the @fold@ value seen from within the 'FoldMapChars'
-- state. It is usually not necessary to invoke this function directly, the definition of
-- 'liftEditLine' for the 'FoldMapLines' function type is this function, so any function that
-- evaluates to an @editor@ where the @editor@ is a member of the 'MonadEditLine' typeclass will
-- automatically invoke this function based on the function type of the context in which it is used.
foldMapChars :: FoldMapChars fold line a -> FoldMapLines fold line a
foldMapChars f = get >>= liftEditText . editLine . runFoldMapChars f >>= state . const

-- | Convert a 'FoldMapChars' into an 'FoldMapLine' function. This function is analogous to the
-- 'runStateT' function.
runFoldMapChars :: FoldMapChars fold line a -> fold -> EditLine line (a, fold)
runFoldMapChars (FoldMapChars f) = runStateT (runExceptT f) >=> \ case
  (Left err, _   ) -> throwError err
  (Right  a, fold) -> return (a, fold)

-- | Like 'runFoldMapChars' but only returns the @fold@ result. This function is analogous to the
-- 'execStateT' function.
execFoldMapChars :: FoldMapChars fold line a -> fold -> EditLine line fold
execFoldMapChars = fmap (fmap snd) . runFoldMapChars

-- | Like 'runFoldMapChars' but ignores the @fold@ result. This function is analogous to the
-- 'evalStateT' function.
evalFoldMapChars :: FoldMapChars fold line a -> fold -> EditLine line a
evalFoldMapChars = fmap (fmap fst) . runFoldMapChars

----------------------------------------------------------------------------------------------------

-- | A type synonym for a 'FoldMapLines' function in which the folded type is the unit @()@ value.
type MapChars = FoldMapChars ()

-- | Evaluate a 'MapChars' using 'evalFoldMapChars'.
runMapChars :: MapChars line a -> EditLine line a
runMapChars = flip evalFoldMapChars ()

----------------------------------------------------------------------------------------------------

-- | This data type stores a buffer of editable text.
data TextBuffer line
  = TextBuffer
    { theBufferCharNumber  :: !Int
      -- ^ The number of characters before the cursor position in the file.
    , theBufferCharCount   :: !Int
      -- ^ The total number of characters in this buffer.
    , theBufferLineCount   :: !Int
      -- ^ The total number of lines in this buffer.
    , theBufferLineNumber  :: !Int
      -- ^ The line number of the 'bufferCurrentLine'. The top-most line of any buffer is 1.
    , theBufferLineBreaker :: LineBreaker
      -- ^ The function used to break strings into lines. This function is called every time a
      -- string is transferred from 'theBufferCursor' to to 'theLinesAbove' or 'theLinesBelow'.
    , theBufferAboveCursor :: !(Seq (TextLine line))
      -- ^ Lines above the cursor
    , theBufferCursor      :: !(TextCursor line)
      -- ^ A data structure for editing individual characters in a line of text.
    , theBufferBelowCursor :: !(Seq (TextLine line))
      -- ^ Lines below the cursor
    }
  deriving Functor

data TextEditError
  = TextEditError StrictBytes
  deriving (Eq, Ord)

-- | A function used to break strings into lines. This function is called every time a string is
-- transferred from 'theBufferCursor' to to 'theLinesAbove' or 'theLinesBelow'.
--
-- If you choose to use your own 'LineBreaker' function, be sure that the function obeys this law:
--
-- @
-- 'Prelude.concat' ('lineBreakNLCR' str) == str
-- @
type LineBreaker = String -> [String]

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
-- 'TextLine', but on a 'Prelude.String' (list of 'Char's) stored in the 'TextBuffer' state.
data TextLine line
  = TextLine
    { theTextLineString :: !StrictBytes
    , theTextLineTags   :: !line
    }
  deriving Functor

-- | Similar to a 'TextLine', but expands the 'textLineString' 
data TextCursor line
  = TextCursor
    { theTextCursorBefore :: String
    , theTextCursorAfter  :: String
    , theTextCursorTag    :: line
    }
  deriving Functor

-- | Use this to initialize a new empty 'TextBuffer'. The default 'bufferLineBreaker' is set to
-- 'lineBreakNLCR'. A 'TextBuffer' always contains one empty line, but a line must have a @line@
-- tag, so it is necessary to pass an initializing tag value of type @line@ -- if you need nothing
-- but plain text editing, @line@ can be unit @()@.
emptyTextBuffer :: line -> TextBuffer line
emptyTextBuffer tag = TextBuffer
  { theBufferCharNumber  = 0
  , theBufferCharCount   = 0
  , theBufferLineNumber  = 0
  , theBufferLineCount   = 0
  , theBufferLineBreaker = lineBreakNLCR
  , theBufferAboveCursor = mempty
  , theBufferCursor      = emptyTextCursor tag
  , theBufferBelowCursor = mempty
  }

-- | Use this to initialize a new empty 'TextCursor'. This is usually only handy if you want to
-- define and test your own 'EditLine' functions and need to evaluate 'runEditLine' by hand rather
-- than allowing the 'TextEdit' APIs automatically manage line editing. A 'TextBuffer' always
-- contains one empty line, but a line must have a @line@ tag, so it is necessary to pass an
-- initializing tag value of type @line@.
emptyTextCursor :: line -> TextCursor line
emptyTextCursor tag = TextCursor
  { theTextCursorBefore = ""
  , theTextCursorAfter  = ""
  , theTextCursorTag    = tag
  }

-- | This is the default line break function. It will split the line on the character sequence
-- @"\n"@, or @"\r"@, or @"\n\r"@, or @"\r\n"@. The line terminators must be included at the end of
-- each broken string, so that the rule that the law @'Prelude.concat' ('lineBreakNLCR' str) == str@
-- is obeyed.
lineBreakNLCR :: String -> [String]
lineBreakNLCR = lines where
  nlcr c = c == '\n' || c == '\r'
  lines = break nlcr >>> \ case
    (""  , "") -> []
    (line, "") -> [line]
    (line, '\n':'\r':more) -> (line ++ "\n\r") : lines more
    (line, '\r':'\n':more) -> (line ++ "\r\n") : lines more
    (line, c:more)         -> [line ++ [c], more]

-- Not for export: updated when characters are added to the 'TextBuffer'.
bufferCharNumber :: Lens' (TextBuffer line) Int
bufferCharNumber = lens theBufferCharNumber $ \ a b -> a{ theBufferCharNumber = b }

-- Not for export: updated when characters are added to the 'TextBuffer'.
bufferCharCount :: Lens' (TextBuffer line) Int
bufferCharCount = lens theBufferCharCount $ \ a b -> a{ theBufferCharCount = b }

-- Not for export: updated when characters are added to the 'TextBuffer'.
bufferLineCount :: Lens' (TextBuffer line) Int
bufferLineCount = lens theBufferLineCount $ \ a b -> a{ theBufferLineCount = b }

-- Not for export: updated when characters are added to the 'TextBuffer'.
bufferLineNumber :: Lens' (TextBuffer line) Int
bufferLineNumber = lens theBufferLineNumber $ \ a b -> a{ theBufferLineNumber = b }

-- | The function used to break strings into lines. This function is called every time a string is
-- transferred from 'theBufferCursor' to 'theLinesAbove' or 'theLinesBelow'. Note that setting this
-- function doesn't restructure the buffer, the old line breaks will still exist as they were before
-- until the entire buffer is refreshed.
--
-- If you choose to use your own 'LineBreaker' function, be sure that the function obeys this law:
--
-- @
-- 'Prelude.concat' ('lineBreakNLCR' str) == str
-- @
bufferLineBreaker :: Lens' (TextBuffer line) LineBreaker
bufferLineBreaker = lens theBufferLineBreaker $ \ a b -> a{ theBufferLineBreaker = b }

-- | Lines above the 'bufferCurrentLine' in the 'TextBuffer'. Note that this list is reversed, so if
-- the current line is at the end of the file, the entire file consists of all lines in this field
-- in reverse order from the direction in which they were read.
bufferAboveCursor :: Lens' (TextBuffer line) (Seq (TextLine line))
bufferAboveCursor = lens theBufferAboveCursor $ \ a b -> a{ theBufferAboveCursor = b }

-- | The current line of text being edited under the cursor.
bufferCurrentLine :: Lens' (TextBuffer line) (TextCursor line)
bufferCurrentLine = lens theBufferCursor $ \ a b -> a{ theBufferCursor = b }

-- | Lines below the 'bufferCurrentLine'.
bufferBelowCursor :: Lens' (TextBuffer line) (Seq (TextLine line))
bufferBelowCursor = lens theBufferBelowCursor $ \ a b -> a{ theBufferBelowCursor = b }

-- | Evaluate an 'EditText' function on the given 'TextBuffer'.
runEditText :: EditText line a -> (TextBuffer line) -> (Either TextEditError a, TextBuffer line)
runEditText (EditText f) = runState $ runExceptT f

-- | The null-terminated, UTF-8 encoded string of bytes stored in this line of text.
textLineString :: Lens' (TextLine line) StrictBytes
textLineString = lens theTextLineString $ \ a b -> a{ theTextLineString = b }

-- | Arbitrary information stored with this text. Typcial use cases for this field may include
-- syntax coloring rules, structured data parsed from the 'textLineString', text search indicies, a
-- diff of changes made, or all of the above.
textLineTags :: Lens' (TextLine line) line
textLineTags = lens theTextLineTags $ \ a b -> a{ theTextLineTags = b }

type RelativeToCursor = forall line . Lens' (TextCursor line) String

-- | A lens that observes or updates characters before the cursor on the 'bufferCurrentLine'. Note
-- that characters observed by this lens are in the reversed of the order in which they were
-- inserted.
textCursorBefore :: RelativeToCursor
textCursorBefore = lens theTextCursorBefore $ \ a b -> a{ theTextCursorBefore = b }

-- | A lens that observes or updates characters after the cursor on the 'bufferCurrentLine'.
textCursorAfter :: RelativeToCursor
textCursorAfter = lens theTextCursorAfter $ \ a b -> a{ theTextCursorAfter = b }

----------------------------------------------------------------------------------------------------

-- | Throughout this module you will find functions that are defined like so:
--
-- @
-- insertChar :: 'MonadEditText' editor => RelativeToCursor -> Char -> editor line ()
-- @
--
-- So you may wonder, when is it possible to use 'insertString' since it's type is the unspecified
-- @editor@ type variable?
--
-- There are two different function types which satisfy the @editor@ type variable: the 'TextEdit'
-- function type which is evaluated by the 'runEditText' function, and the 'TextFoldMap' function
-- type which is evaluated by the 'runTextFoldMap' function.
--
-- So any function type you see in this module that evaluates to a polymorphic type variable
-- @editor@, where @editor@ is a member of 'MonadEditText' (for example 'insertString'), can be used
-- when building either a 'TextEdit' or 'TextFoldMap' function that is then passed as a parameter to
-- the 'runEditText' or 'runTextFoldMap' (respectively).
--
-- This design pattern is similar to 'Control.Monad.IO.Class.liftIO', and then defining an API in
-- which all functions evaluate to a function of type @'Control.Monad.IO.Class.MonadIO' m => m a@,
-- which allows you to evaluate any of these API functions in any monadic function context (usually
-- a @do@ block of code) without needing to explicitly call 'liftIO', which is possible as long as
-- that monadic context is a member of the 'Control.Monad.IO.Class.MonadIO' typeclass. Likewise,
-- most API functions in this module can be evauated in any monadic context without needing to
-- explicitly call 'liftEditText'.
class MonadEditText m where
  liftEditText :: EditText line a -> m line a

instance MonadEditText EditText where { liftEditText = id; }
instance MonadEditText (FoldMapLines fold) where { liftEditText = FoldMapLines . lift . lift; }

-- | This class is basically the same as 'MonadEditText', but lifts a 'EditLine' function rather
-- than an 'EditText' function. Note that 'MonadEditText' is a subclass of this typeclass, which
-- means if you 'liftEditChar' should work anywhere a 'liftEditText' function will work.
class MonadEditLine m where
  liftEditLine :: EditLine line a -> m line a

instance MonadEditLine EditLine where { liftEditLine = id; }
instance MonadEditLine (FoldMapChars fold) where { liftEditLine = FoldMapChars . lift . lift; }
instance MonadEditLine EditText where { liftEditLine = editLine; }
instance MonadEditLine (FoldMapLines fold) where { liftEditLine = foldMapChars . liftEditLine; }

----------------------------------------------------------------------------------------------------


