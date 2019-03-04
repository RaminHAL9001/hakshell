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
    MonadEditText(..), MonadEditLine(..),
    RelativeToCursor(..),
    clearCurrentLine, insertChar, deleteChars, deleteCharsWrap, insertString, 
    -- * Text Editor Function Types
    EditText, runEditText, newTextBuffer, newTextCursor,
    FoldMapLines, runFoldMapLines, execFoldMapLines, evalFoldMapLines,
    MapLines, runMapLines,
    EditLine, editLine,
    FoldMapChars, foldMapChars, runFoldMapChars, execFoldMapChars, evalFoldMapChars,
    MapChars, runMapChars,
    -- * Text Editor Data Structures
    -- ** Text Buffer
    TextBuffer, LineBreaker(..), lineBreaker, lineBreakPredicate,
    theBufferCharCount, theBufferCharNumber,
    theBufferLineCount, theBufferLineNumber,
    bufferCurrentLine, textLineString,
    lineBreakNLCR,
    -- ** Line Editing
    TextLine, TextCursor, textCursorCharCount, textLineTags,
    -- ** Errors
    TextEditError(..),
    -- * Re-exporting "Hakshell.String"
    module Hakshell.String
  ) where

import           Hakshell.String

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Primitive
import           Control.Monad.State

import qualified Data.ByteString             as Strict
import qualified Data.ByteString.UTF8        as UTF8st
import qualified Data.ByteString.Lazy        as Lazy
import qualified Data.ByteString.Lazy.UTF8   as UTF8lz
import           Data.Foldable               (toList)
import           Data.Semigroup
import qualified Data.Sequence               as Seq
import qualified Data.Vector.Mutable         as MVec
import qualified Data.Vector.Generic.Mutable as GMVec
import qualified Data.Vector.Unboxed.Mutable as UMVec

----------------------------------------------------------------------------------------------------

-- | This is a type of functions that can modify the textual content stored in a 'TextBuffer'.
newtype EditText line a
  = EditText{ unwrapEditText :: ExceptT TextEditError (StateT (TextBuffer line) IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState (TextBuffer line) (EditText line) where { state = EditText . lift . state; }

instance MonadError TextEditError (EditText line) where
  throwError = EditText . throwError
  catchError (EditText try) catch = EditText $ catchError try $ unwrapEditText . catch

-- | Evaluate an 'EditText' function on the given 'TextBuffer'.
runEditText :: EditText line a -> TextBuffer line -> IO (Either TextEditError a, TextBuffer line)
runEditText (EditText f) = runStateT $ runExceptT f

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
  deriving (Functor, Applicative, Monad, MonadIO)

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
  deriving (Functor, Applicative, Monad, MonadIO)

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
  deriving (Functor, Applicative, Monad, MonadIO)

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
    , theBufferDefaultTag  :: line
      -- ^ The tag value to use when new 'TextLine's are automatically constructed after a line
      -- break character is inserted.
    , theBufferLineBreaker :: LineBreaker
      -- ^ The function used to break strings into lines. This function is called every time a
      -- string is transferred from 'theBufferCursor' to to 'theLinesAbove' or 'theLinesBelow'.
    , theBufferVector      :: !(MVec.IOVector (TextLine line))
      -- ^ A mutable vector containing each line of editable text.
    , theLinesAboveCursor  :: !Int
      -- ^ The number of lines above the cursor
    , theLinesBelowCursor  :: !Int
      -- ^ The number of line below the cursor
    , theBufferCursor      :: !(TextCursor line)
      -- ^ A data structure for editing individual characters in a line of text.
    }

data TextEditError
  = TextEditError StrictBytes
  deriving (Eq, Ord)

-- | A pair of functions used to break strings into lines. This function is called every time a
-- string is transferred from 'theBufferCursor' to to 'theLinesAbove' or 'theLinesBelow' to ensure
-- all strings entered into a buffer have no more than one line terminating character sequence at
-- the end of them.
--
-- If you choose to use your own 'LineBreaker' function, be sure that the function obeys this law:
--
-- @
-- 'Prelude.concat' ('lineBreakNLCR' str) == str
-- @
data LineBreaker
  = LineBreaker
    { theLineBreakPredicate :: Char -> Bool
      -- ^ This function is called by 'insertChar' to determine if the 'bufferCurrentLine' should be
      -- terminated.
    , theLineBreaker :: String -> [String]
      -- ^ This function scans through a string finding character sequences that delimit the end of
      -- a line of text.
    }

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

-- | The current line that is being edited.
data TextCursor line
  = TextCursor
    { theLineEditBuffer    :: !(UMVec.IOVector Char)
    , theCharsBeforeCursor :: !Int
    , theCharsAfterCursor  :: !Int
    , theTextCursorTag     :: line
    }
  deriving Functor

-- Not for export: this buffer is formatted such that characters before the cursror are near index
-- zero, while characters after the cursor are near the final index.
lineEditBuffer :: Lens' (TextCursor line) (UMVec.IOVector Char)
lineEditBuffer = lens theLineEditBuffer $ \ a b -> a{ theLineEditBuffer = b }

-- Not for export: this gets updated when inserting or deleting characters before the cursor.
charsBeforeCursor :: Lens' (TextCursor line) Int
charsBeforeCursor = lens theCharsBeforeCursor $ \ a b -> a{ theCharsAfterCursor = b }

-- Not for export: this gets updated when inserting or deleting characters after the cursor.
charsAfterCursor :: Lens' (TextCursor line) Int
charsAfterCursor = lens theCharsAfterCursor $ \ a b -> a{ theCharsAfterCursor = b}

-- | Use this to initialize a new empty 'TextBuffer'. The default 'bufferLineBreaker' is set to
-- 'lineBreakNLCR'. A 'TextBuffer' always contains one empty line, but a line must have a @line@
-- tag, so it is necessary to pass an initializing tag value of type @line@ -- if you need nothing
-- but plain text editing, @line@ can be unit @()@.
newTextBuffer :: line -> IO (TextBuffer line)
newTextBuffer tag = do
  cur <- newTextCursor tag
  buf <- MVec.new 512
  return TextBuffer
    { theBufferCharNumber  = 0
    , theBufferCharCount   = 0
    , theBufferLineNumber  = 0
    , theBufferLineCount   = 0
    , theBufferDefaultTag  = tag
    , theBufferLineBreaker = lineBreakNLCR
    , theBufferVector      = buf
    , theLinesAboveCursor  = 0
    , theLinesBelowCursor  = 0
    , theBufferCursor      = cur
    }

-- | Use this to initialize a new empty 'TextCursor'. This is usually only handy if you want to
-- define and test your own 'EditLine' functions and need to evaluate 'runEditLine' by hand rather
-- than allowing the 'TextEdit' APIs automatically manage line editing. A 'TextBuffer' always
-- contains one empty line, but a line must have a @line@ tag, so it is necessary to pass an
-- initializing tag value of type @line@.
newTextCursor :: line -> IO (TextCursor line)
newTextCursor tag = do
  buf <- UMVec.replicate 1024 '\0'
  return TextCursor
    { theLineEditBuffer    = buf
    , theCharsBeforeCursor = 0
    , theCharsAfterCursor  = 0
    , theTextCursorTag     = tag
    }

-- | Determine how many characters have been stored into this buffer.
textCursorCharCount :: TextCursor line -> Int
textCursorCharCount cur = theCharsBeforeCursor cur + theCharsAfterCursor cur

-- Not for export: unsafe because it does not check for line breaks in the given string. This
-- function copies the characters from a 'TextCursor' buffer into a pure 'TextLine'.
unsafeMakeLine :: TextCursor line -> IO (TextLine line)
unsafeMakeLine cur = do
  let buf = theLineEditBuffer cur
  let len = UMVec.length buf
  let chars = mapM $ UMVec.read buf
  before <- chars [0  .. theCharsBeforeCursor cur - 1] 
  after  <- chars [len - theCharsAfterCursor  cur .. len - 1]
  return TextLine
    { theTextLineString = packSize (textCursorCharCount cur) $ before ++ after
    , theTextLineTags   = theTextCursorTag cur
    }

-- | This is the default line break function. It will split the line on the character sequence
-- @"\n"@, or @"\r"@, or @"\n\r"@, or @"\r\n"@. The line terminators must be included at the end of
-- each broken string, so that the rule that the law @'Prelude.concat' ('lineBreakNLCR' str) == str@
-- is obeyed.
lineBreakNLCR :: LineBreaker
lineBreakNLCR = LineBreaker
  { theLineBreakPredicate = nlcr
  , theLineBreaker = lines
  } where
    nlcr c = c == '\n' || c == '\r'
    lines  = break nlcr >>> \ case
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

-- | Entering a line-breaking character (e.g. @'\n'@) into a 'TextBuffer' using 'insertChar' or
-- 'insertString' results in several 'TextLine's being generated automatically. Whenever a
-- 'TextLine' is constructed, there needs to be a default tag value that is assigned to it. This
-- lens allows you to observe or set the default tag value.
bufferDefaultTag :: Lens' (TextBuffer line) line
bufferDefaultTag = lens theBufferDefaultTag $ \ a b -> a{ theBufferDefaultTag = b }

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
bufferLineBreaks :: Lens' (TextBuffer line) LineBreaker
bufferLineBreaks = lens theBufferLineBreaker $ \ a b -> a{ theBufferLineBreaker = b }

-- | This function is called by 'insertChar' to determine if the 'bufferCurrentLine' should be
-- terminated.
lineBreakPredicate :: Lens' LineBreaker (Char -> Bool)
lineBreakPredicate = lens theLineBreakPredicate $ \ a b -> a{ theLineBreakPredicate = b }

-- | This function scans through a string finding character sequences that delimit the end of a line
-- of text.
lineBreaker :: Lens' LineBreaker (String -> [String])
lineBreaker = lens theLineBreaker $ \ a b -> a{ theLineBreaker = b }

-- Not for export: requires correct accounting of line numbers to avoid segment faults.
linesAboveCursor :: Lens' (TextBuffer line) Int
linesAboveCursor = lens theLinesAboveCursor $ \ a b -> a{ theLinesAboveCursor = b }

-- Not for export: requires correct accounting of line numbers to avoid segment faults.
linesBelowCursor :: Lens' (TextBuffer line) Int
linesBelowCursor = lens theLinesBelowCursor $ \ a b -> a{ theLinesBelowCursor = b }

-- Not for export: the vector containing all the lines of text in this buffer.
bufferVector :: Lens' (TextBuffer line) (MVec.IOVector (TextLine line))
bufferVector = lens theBufferVector $ \ a b -> a{ theBufferVector = b }

-- | The current line of text being edited under the cursor.
bufferCurrentLine :: Lens' (TextBuffer line) (TextCursor line)
bufferCurrentLine = lens theBufferCursor $ \ a b -> a{ theBufferCursor = b }

-- | The null-terminated, UTF-8 encoded string of bytes stored in this line of text.
textLineString :: Lens' (TextLine line) StrictBytes
textLineString = lens theTextLineString $ \ a b -> a{ theTextLineString = b }

-- | Arbitrary information stored with this text. Typcial use cases for this field may include
-- syntax coloring rules, structured data parsed from the 'textLineString', text search indicies, a
-- diff of changes made, or all of the above.
textLineTags :: Lens' (TextLine line) line
textLineTags = lens theTextLineTags $ \ a b -> a{ theTextLineTags = b }

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

-- | Controls whether characters are inserted/deleted before or after the cursor.
data RelativeToCursor = Before | After
  deriving (Eq, Ord, Read, Show, Bounded, Enum)

-- Not for export: This function takes a 'RelativeToCursor' value and constructs a lens that can be
-- used to access 'TextLine's within the 'TextCursor'.
relativeToLine :: RelativeToCursor -> Lens' (TextBuffer line) Int
relativeToLine = \ case { Before -> linesAboveCursor; After -> linesBelowCursor; }
  -- This function may dissapear if I decide to buffer lines in a mutable vector.

-- Not for export: This function takes a 'RelativeToCursor' value and constructs a lens that can be
-- used to access a character index within the 'bufferCurrentLine'.
relativeToChar :: RelativeToCursor -> Lens' (TextBuffer line) Int
relativeToChar =
  (bufferCurrentLine .) . \ case { Before -> charsBeforeCursor; After -> charsAfterCursor; }

-- Get an index into the 'bufferVector' from the current position of the cursor.
cursorIndex :: RelativeToCursor -> EditText line Int
cursorIndex = \ case
  Before -> use linesAboveCursor
  After  -> (-) <$> (MVec.length <$> use bufferVector) <*> (subtract 1 <$> use linesBelowCursor)

-- Not for export: unsafe, requires correct accounting of cursor positions, otherwise segfaults may
-- occur. Cursor is implemented by keeping a mutable array in which elements before the cursor fill
-- the array from index zero up to the cursor, and the elements after the cursror fill the array
-- from the top-most index of the array down to the index computed from the top-most index of the
-- array subtracted by the total number of elements in the array plus the cursor position.
growVec
  :: (GMVec.MVector vector a, PrimMonad m)
  => vector (PrimState m) a
  -> Int -> Int -> Int -> m (vector (PrimState m) a)
growVec vec beforeElems afterElems addElems = do
  let reqSize = beforeElems + afterElems + addElems
  let len = GMVec.length vec
  if reqSize <= len then return vec else do
    let newSize = head $ dropWhile (< reqSize) $ iterate (* 2) len
    newVec <- GMVec.new newSize
    GMVec.copy (GMVec.slice 0 beforeElems newVec) (GMVec.slice 0 beforeElems vec)
    GMVec.copy
      (GMVec.slice (len - afterElems) (len - 1) newVec)
      (GMVec.slice (len - afterElems) (len - 1) vec)
    return newVec

-- Not for export: should be executed automatically by insertion operations. Increases the size of
-- the 'lineEditBuffer' to be large enough to contain the current 'textCursorCharCount' plus the
-- given number of elements.
growLineIfTooSmall :: Int -> EditText line ()
growLineIfTooSmall grow = do
  cur <- use bufferCurrentLine
  buf <- liftIO $
    growVec (cur ^. lineEditBuffer) (cur ^. charsBeforeCursor) (cur ^. charsAfterCursor) grow
  bufferCurrentLine . lineEditBuffer .= buf

growBufferIfTooSmall :: Int -> EditText line ()
growBufferIfTooSmall grow = do
  buf   <- use bufferVector
  above <- use linesAboveCursor
  below <- use linesBelowCursor
  liftIO (growVec buf above below grow) >>= assign bufferVector

-- Not for export: these functions, when evaluated alone, leave the 'TextBuffer' in an inconsistent
-- state. This function creates a 'TextLine' from the 'TextCursor' stored at 'bufferCurrentLine'. It
-- does nothing to replace the 'bufferCurrentLine', so essentially the line is duplicated and pushed
-- upward or downward, and it is then up to the calling context whether to open a new line, or
-- replace the current line with a line above or below.
pushCurrentLine :: RelativeToCursor -> EditText line ()
pushCurrentLine rel = growBufferIfTooSmall 1 >>
  (MVec.write <$> use bufferVector <*> cursorIndex rel <*>
    (use bufferCurrentLine >>= liftIO . unsafeMakeLine)) >>= liftIO

----------------------------------------------------------------------------------------------------

-- | Delete the current line, replacing it with a new empty line.
clearCurrentLine :: (MonadEditText editor, Monad (editor line)) => editor line ()
clearCurrentLine = liftEditText $
  use bufferDefaultTag >>= liftIO . newTextCursor >>= assign bufferCurrentLine

-- | Insert a single character. If the 'lineBreakPredicate' function evaluates to 'Prelude.True',
-- meaning the given character is a line breaking character, this function does nothing. To insert
-- line breaks, using 'insertString'.
insertChar
  :: (MonadEditText editor, Monad (editor line))
  => RelativeToCursor -> Char -> editor line ()
insertChar rel c = liftEditText $ do
  isBreak <- use $ bufferLineBreaks . lineBreakPredicate
  if isBreak c then return () else do
    growLineIfTooSmall 1
    cur <- use bufferCurrentLine
    let buf = cur ^. lineEditBuffer
    let len = UMVec.length buf
    let (diff, topMinus) = case rel of { Before -> (1, id); After -> ((-1), (len -)); }
    i   <- topMinus <$> use (relativeToChar rel)
    buf <- use $ bufferCurrentLine . lineEditBuffer
    liftIO $ UMVec.write buf i c
    relativeToChar rel += diff

-- | This function only deletes characters on the current line, if the cursor is at the start of the
-- line and you evaluate @'deleteChars' 'Before'@, this function does nothing.
deleteChars
  :: (MonadEditText editor, Monad (editor line))
  => RelativeToCursor -> Int -> editor line ()
deleteChars rel n = liftEditText $ relativeToChar rel %= max 0 . subtract (max 0 n)

-- | This function deletes characters starting from the cursor, and if the number of characters to
-- be deleted exceeds the number of characters in the current line, characters are deleted from
-- adjacent lines such that the travel of deletion wraps to the end of the prior line or the
-- beginning of the next line, depending on the direction of travel.
deleteCharsWrap
  :: (MonadEditText editor, Monad (editor line))
  => RelativeToCursor -> Int -> editor line ()
deleteCharsWrap = error "TODO: deleteCharsWrap"

-- | This function evaluates the 'lineBreaker' function on the given string, and beginning from the
-- current cursor position, begins inserting all the lines of text produced by the 'lineBreaker'
-- function.
insertString
  :: (MonadEditText editor, Monad (editor line))
  => RelativeToCursor -> Int -> editor line ()
insertString = error "TODO: insertString"
