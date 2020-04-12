-- | This module integrates text processing facilities into Hakshell.
--
-- * Preface
--
-- Please skip to the API documentation below if you simply want to know how to use this module.
--
-- ** Why a Hakshell needs a built-in editor
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
-- distinction between a shell and an editor is really not so distinct. Binary data of course can
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
--- The "Hakshell.TextEditor" module is therefore designed to provide a solid, consistent foundation
--- for automated text processing APIs, upon which programmers can build text editors and text
--- processors, and hence a text editor API is an integral part of the "Hakshell" library.
module Hakshell.TextEditor
  ( -- * Text Buffers

    -- ** Cursors
    --
    -- The concept of a "cursor" is important to understand in order to use the functions in this
    -- module. The 'EditText' function type and the 'ViewText' function type both have cursors which
    -- can be moved around a 'TextBuffer' or 'TextFrame' (respectively), and many functions can
    -- copy or fold over text relative to that cursor position.
    --
    -- All functions which have access to cursors in their context provide the ability to query the
    -- location of the cursor. But the ability to edit text under the cursor is unique to the
    -- 'EditText' function.

    CursorIndexedText(..), CursorIndexedLine(..),

    -- ** The 'TextLine' data type
    --
    -- A 'TextBuffer' is a memory-efficient vector containing 'TextLine's. A 'TextLine' is created
    -- by evaluating a 'TextEdit' function such as 'insertString'. A 'TextLine' within a
    -- 'TextBuffer' can be updated by navigating the cursor to the line using 'gotoChar' or
    -- 'moveByChar'. Moving the cursor to an address (addresses are line numbers) will copy the
    -- 'TextLine' at that address into a 'LineEditor'. You can then edit the text in a 'LineEditor'
    -- by evaluating a function of type 'EditLine' with the 'editLine' function.

    TextLine, emptyTextLine, textLineBreakSymbol,
    nullTextLine, textLineCursorSpan, textLineGetChar,
    textLineTop, textLineIsUndefined,
    textLineTags, textLineChomp, showTextLine,

    -- *** 'TextLine' Inspection Monad
    ViewLine, liftViewLine, runViewLineT, runViewLine, viewerTopChar, cursorIsOnLineBreak,
    viewerCursorToChar, sliceLineToEnd, splitLine,

    -- ** The 'TextBuffer' data type
    --
    -- A 'TextBuffer' can be created with 'newTextBuffer', and then edited by evaluating a function
    -- of type 'EditText' on it with the 'runEditTextIO' function. You typically fill a 'TextBuffer'
    -- with a "String" using 'insertString'. A 'TextBuffer' contains a cursor which you can move
    -- around using the 'gotoChar', 'moveByChar', functions. Text is deleted with the
    -- 'deleteCharsWrap' functions.

    TextBuffer, TextIORef, TextMutex, TextSTRef,
    newTextBuffer, copyTextBuffer,
    bufferLineBreaker, bufferDefaultTags, bufferTargetCol,
    thisTextBuffer,

    module Hakshell.TextEditor.LineBreaker,

    -- ** The 'LineEditor' data type.
    --
    -- Every 'TextBuffer' has it's own 'LineEditor' which can be updated by evaluating a function of
    -- type 'EditLine' using the 'editLine' function.

    LineEditor,
    flushRefill, refillLineEditor, refillLineEditorWith, flushLineEditor,

    -- *** Working with the content of a 'LineEditor'
    lineEditorCharCount, lineEditorUnitCount, getLineCharCount, getUnitCount,
    copyLineEditorText, clearLineEditor, resetLineEditor,

    -- ** The 'EditText' function type
    --
    -- Note that some functions of the 'EditText' function type, like 'getLineIndex',
    -- 'getLineNumber', 'getColumnNumber', are defined as members of the 'CursorIndexedText'
    -- typeclass.

    EditText, runEditText, runEditTextOnCopy,

    insertString, lineBreak, lineBreakWith,
    pushLine, popLine, getColumnNumber,
    putLineIndex, withCurrentLine,
    bufferLineCount, bufferIsEmpty, currentBuffer,
    lineCursorIsDefined, 

    -- ** The 'EditLine' function type
    --
    -- Usually, functions of type 'EditLine' are evaluated by using the 'editLine' function within
    -- the context of an 'EditText' function. This places a line of text into a 'LineEditor' which
    -- allows the text to be edited character-by-character.

    EditLine, editLine, liftEditText, insertChar, deleteChars, lineEditorTags,
    copyCharsRange, copyCharsBetween, copyChars, copyCharsToEnd,
    charCursorIsDefined,
    -- TODO: getCharIndex, putCharIndex,

    -- ** Indicies and Bounds Checking

    validateLineIndex, lineIndexIsValid,
    validateGetLineIndex, testLocation, validateLocation, locationIsValid,
    validateCharIndex, indexIsOnLineBreak,

    -- *** Error Data Type

    TextEditError(..),
    
    -- ** Positioning the Cursor
    --
    -- __IMPORTANT__: when moving the cursor around, some functions, like 'gotoLine' or 'moveByLine'
    -- do not leave it's contents on the current line unless you explicitly call 'flushLineEditor',
    -- but other functions like 'gotoLocation' do automatically call 'flushLineEditor'. Without
    -- "flushing" the characters currently in the 'LineEditor' will not be saved to the 'TextBuffer'
    -- and will be carried around to the 'LineEditor's current location, however if the cursor's
    -- column location cannot be changed unless 'refilLineEditor' is used to move the line
    -- location.
    --
    -- There are situations where you may not want to use 'flushRefill' to perform a cursor motion, as
    -- in when accumulating lines into the 'LineEditor' after each cursor motion.

    getLocation, gotoLocation, saveCursorEval, gotoLine, goNearLine, gotoChar, goNearChar,

    Absolute(..),  LineIndex(..), CharIndex(..), TextLocation(..),
    TextCursorSpan(..), CharStats(..),
    shiftAbsolute, diffAbsolute, unwrapTextCursorSpan,
    sumTextLocation, diffTextLocation,

    -- *** Moving the cursor relative to the cursor

    moveCursor, moveByLine, moveByChar,

    Relative(..), RelativeToCursor(..),
    RelativeToAbsoluteCursor, -- <- does not export members
    relativeToAbsolute, relativeLine, relativeChar, lineIndex, charIndex,

    -- ** Text Frames
    --
    -- A 'TextFrame' is an immutable copy of a portion of a 'TextBuffer'. By creating a 'TextFrame'
    -- you take a snapshot of a 'TextBuffer' at a point in time which can then be pasted into
    -- another 'TextBuffer', or used to draw the window of an interactive text editor application.

    TextFrame, textFrame, textFrameOnLines, textFrameAppend, emptyTextFrame,
    newTextBufferFromFrame, textFrameCharCount, textFrameVector,
    textFrameToList, textFrameToStrings,

    -- *** 'TextFrame' Inspection Monad
    ViewText, runViewTextT, runViewText, viewLine, viewerCursorToLine, viewerGetLine,

    -- * Batch Editing

    -- ** Only folding over lines of text
    --
    -- These functions perform a batch read-only opertion over the 'TextBuffer' without moving the
    -- position of the cursor. Be careful to evaluate 'flushLineEditor' before evaluating folds over
    -- 'TextBuffer's to ensure the latest changes to the 'LineEditor' are actually stored into the
    -- 'TextBuffer' and are ready to be folded, or your results may not be what you expect.

    FoldLines, foldLines, foldAllLines, foldLinesBetween, foldLinesInRange, runFoldLinesStep,

    -- ** Only mapping over lines  of text
    --
    -- Folding and mapping can both be done in a single pass. It is also possible to halt a
    -- folding/mapping function by evaluating a halting continuation function provided by
    -- 'forLinesInRange', 'forLines', and 'forLinesInBuffer'. Functions of the type described here
    -- are used to perform statelses updates on a buffer, for example a context-free search and
    -- replace function.

    MapLines, mapLines, mapAllLines, mapLinesBetween, mapLinesInRange, runMapLinesStep,

    -- ** A Function Type for Both Folding and Mapping

    FoldMapLines, foldMapLines, foldMapAllLines, foldMapLinesBetween, foldMapLinesInRange,
    runFoldMapLinesStep,

    -- * Parser Stream
    --
    -- To perform parsing of text, use the "Hakshell.TextEditor.Parser" module. The 'StreamCursor'
    -- provided here provides stateful information necessary to efficiently deliver a stream of
    -- characters from a 'TextBuffer' to a 'Hakshell.TextEditor.Parser.Parser'.

    StreamCursor, newStreamCursorRange, newStreamCursor,
    streamGoto, streamLook, streamStep, streamIsEOF, streamIsEOL,
    streamTags, streamCommitTags, streamResetCache, streamResetEndpoint,
    theStreamCache, theStreamLocation, theStreamEndpoint,

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

import           Hakshell.GapBuffer
import           Hakshell.String
import           Hakshell.TextEditor.LineBreaker

import           Control.Arrow
import           Control.Exception
import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.Cont.Class
import           Control.Monad.Except
import           Control.Monad.Error.Class
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State
import           Control.Monad.State.Class

import           Data.Primitive.MutVar       (MutVar, newMutVar, readMutVar, writeMutVar)
import           Data.Primitive.MVar         (MVar, newMVar, readMVar, takeMVar, putMVar)
import qualified Data.Vector                 as Vec
--import qualified Data.Vector.Generic         as GVec
import qualified Data.Vector.Mutable         as MVec
import qualified Data.Vector.Unboxed         as UVec
import qualified Data.Vector.Unboxed.Mutable as UMVec

----------------------------------------------------------------------------------------------------

-- Used by 'newTextBuffer' as a parameter to 'newTextBufferState'.
defaultInitTextBufferSize :: Int
defaultInitTextBufferSize = 512

defaultInitLineBufferSize :: Int
defaultInitLineBufferSize = 1024

----------------------------------------------------------------------------------------------------

-- | A number for indexing a line. This data type instantiates the 'Prelude.Num' typeclass so that
-- you can write an integer literal in your code and (if used in the correct context) the type
-- inference will automatically declare a 'LineIndex' without you needing to write @(LineIndex 1)@
-- constructor unless you really want to.
newtype LineIndex = LineIndex Int
  deriving (Eq, Ord, Show, Read, Enum, Num, Bounded)

instance IsIndex (Absolute LineIndex) where
  fromIndex (Absolute (LineIndex i)) = i - 1
  toIndex = Absolute . LineIndex . (+ 1)

instance IsLength (Relative LineIndex) where
  fromLength (Relative (LineIndex i)) = i
  toLength = Relative . LineIndex

-- | A number for indexing a column, i.e. a character within a line. This data type instantiates
-- the 'Prelude.Num' typeclass so that you can write an integer literal in your code and (if used in
-- the correct context) the type inference will automatically declare a 'CharIndex' without you
-- needing to write @(LineIndex 1)@ constructor unless you really want to.
newtype CharIndex = CharIndex Int
  deriving (Eq, Ord, Show, Read, Enum, Num, Bounded)

instance IsIndex (Absolute CharIndex) where
  fromIndex (Absolute (CharIndex i)) = i - 1
  toIndex = Absolute . CharIndex . (+ 1)

instance IsLength (Relative CharIndex) where
  fromLength (Relative (CharIndex i)) = i
  toLength = Relative . CharIndex

-- | When instructing the editor engine to move by or delete a number of cursor locations (where
-- line breaking characters consisting of two characters are considered a single cursor location,
-- thus the 'moveByCharWrap' or 'deleteCharsWrap' functions), you must specify the number of cursor
-- locations to move or delete using a value of this type.
newtype TextCursorSpan = TextCursorSpan Int
  deriving (Eq, Ord, Show, Read, Enum, Num, Bounded)

-- (( Programmer notes ))
--
-- (o) This is a Gap Buffer
--
-- This module implements a mutable "gap buffer" data structure, using the @vector@ library. The
-- underlying vector type is polymorphic so that elements can be boxed or unboxed. A line editor is
-- an mutable gap buffer of unboxed 'Char' values, a text editor is a mutable gap buffer of boxed
-- 'TextLine' values. 'TextLine' values are ordinary data types which contain references to unboxed
-- immutable vectors of characters. The 'TextLine' is a sum type similar to 'Maybe' in that there is
-- a null 'TextLineUndefined' constructor used to instantiate an empty buffer.
--
-- Mutable buffers are pre-allocated to some exponent of 2 so lines being added does not always
-- result in a re-allocation of the whole buffer. The buffer usually contains 'TextLineUndefined'
-- lines. For efficient insertion, lines below the cursor are shifted toward the end of the vector,
-- leaving a gap of many conecutive 'TextLineUndefined' lines which can be over-written in O(1)
-- time. This is called a "gap buffer" data structure.
--
-- (o) Privately 0-based indicies, publicly 1-based indicies
--
-- When a user locations the cursor, it is done with 1-based indexing values of type 'LineIndex' or
-- 'CharIndex', so the first line is line 1, not line zero. This means care must be taken to
-- translate these addresses to vector indicies by subtracting 1 from every line number parameter
-- passed by a public API, which is done with functions like 'charToIndex' and
-- 'lineToIndex'.
--
-- Another important thing to remember is that users express __inclusive__ line ranges. So when
-- selecting "from lines 2 to 3," the user expects both lines 2 and 3, so subtract 3 from 2 and add
-- 1 to get the total number of lines in the range. There are no semantics for a range of zero
-- lines, "lines 2 to 2" means select only line 2, a single line. Conceptually the end of the range
-- "from lines 2 to 3" means the cursor is between lines 3 and 4, so when translating this to vector
-- indicies, you want the cursor to end at just above line 4. If the buffer only contains 4 lines,
-- index 4 does not exist but it is still valid for computing the range of lines.
--
-- (o) Schrodinger's cursor: the element under the cursor is in a "super-position" state
--
-- In a gap buffer, there is a "cursor" indicating to which index within the vector that the next
-- element will be stored. But there is a question of how to handle the element under the cursor
-- itself. In an empty buffer, there are zero elements before the cursor, and the element under the
-- cursor is undefined. We must also consider how to fill the buffer with elements, understanding
-- that incrementing the cursor and then writing the next element to the cursor is a two-step
-- process. In between these steps (increment cursor, /then/ write to cursor) the cursor index
-- points to an undefined element. However after writing to the cursor, the element is defined.
--
-- There are three ways to approach this problem:
--
-- 1. Make the cursor index and the number of elements before the cursor separate values. When the
--    values are equivalent, the item under the cursor is valid. This makes it easier to do
--    accounting of how many elements there are, but it is important to not confuse the field that
--    tracks how many elements there are before the cursor with the cursor index iteself (using
--    different types would be helpful). It is also important to increment each value at the correct
--    time.
--
-- 2. Keep a boolean value indicating whether the element under the cursor is valid. When counting
--    elements, the boolean must be consulted and the sum total must be incremented when the boolean
--    is set to true. There is no confusing which field is the cursor index and which field is the
--    count of elements before the cursor. With approach (1) above it is important to increment each
--    fields at the correct time, likewise with this approach (2) is is equally important to set and
--    unset the boolean at the correct time.
--
-- 3. Ensure that there is always one valid element in the buffer, and when the buffer is emptied,
--    or when the cursor is incremented, immediately write a default ('mempty'-like) value to the
--    cursor to ensure accessing the element under the cursor returns a defined value. This approach
--    makes sense for the line buffer, but not so much for the character buffer, as you will tend to
--    receive a string with a single '\0' value rather than an empty string when copying elements up
--    to the cursor.
--
-- I decided on approach (2). This is because it is easy to conditionally add one to an element
-- count, and it is easy to determine whether the element under the cursor is valid by checking as
-- single field.

unwrapTextCursorSpan :: TextCursorSpan -> Int
unwrapTextCursorSpan (TextCursorSpan o) = o

-- | This data structure contains information about the number of character locations traversed
-- during a function evaluation. Values of this type are returned by 'insertString',
-- 'deleteCharsWrap', or 'moveByCharWrap'. It counts both actual UTF characters and "cursor steps".
-- A "cursor step" is a unit counted in a way that treats multiple-character line breaks like "\r\n"
-- or "\n\r" as a single unit. The function 'textLineCursorSpan' and 'lineEditorUnitCount' return
-- the logical character counts contained within their respective data structures. If you document
-- is defined such that all line breaking characters in your document must be "\n", then
-- 'cursorStepCount' will always be equal to 'deltaCharCount'.
--
-- For example, if you set the 'TextBuffer's 'bufferLineBreaker' field to 'lineBreakerNLCR':
-- (@bufferLineBreaker .= lineBreakerNLCR@) and then evaluate 'insertString' on the string
-- @"\\r\\n\\r\\n\\r\\n"@, the 'insertString' function will return a count of 3 for the
-- 'cursorStepCount' and a count of 6 for the 'deltaCharCount'.
data CharStats
  = CharStats
    { cursorStepCount :: !TextCursorSpan
      -- ^ When an 'TextBuffer' stateful operation produces a value of this type, it is an
      -- indication of the minimum number of cursor movements that would have had to been made by an
      -- end user pressing keyboard keys (e.g. pressing the right-arrow to perform a traversal) in
      -- order to replicate the stateful operation. The biggest difference between this number and
      -- the 'deltaCharCount' is that line breaks are always considered a single cursor step,
      -- regardless of how many characters comprise the line break.
    , deltaCharCount  :: !(Relative CharIndex)
      -- ^ This is the number of UTF characters inserted\/deleted, a positive value indicates
      -- insertion, a negative value indicates deletion. Use this value if you need an accurate
      -- accounting of the amount of data has been changed.
    }
  deriving Show

instance Semigroup CharStats where
  (<>) (CharStats{cursorStepCount=csA,deltaCharCount=dcA})
       (CharStats{cursorStepCount=csB,deltaCharCount=dcB}) = CharStats
         { cursorStepCount = csA + csB
         , deltaCharCount  = dcA + dcB
         }

instance Monoid CharStats where
  mempty = CharStats{ cursorStepCount = 0, deltaCharCount = 0 }
  mappend = (<>)

-- | This function is an arithmetic operation that alters an 'Absolute' index by adding a 'Relative'
-- index to it.
shiftAbsolute :: Num n => Absolute n -> Relative n -> Absolute n
shiftAbsolute (Absolute a) (Relative b) = Absolute (a + b)

-- | This function is an arithmetic operation that produces a 'Relative' index value by computing
-- the difference between two 'Absolute' index values. The arguments to this function are similar to
-- that of the 'subtract' function, i.e. if you compute @'diffAbsolute' a b@ where @a@ is smaller
-- than @b@ the result is a positive 'Relative' index value.
diffAbsolute :: Num n => Absolute n -> Absolute n -> Relative n
diffAbsolute (Absolute a) (Absolute b) = Relative (subtract a b)

----------------------------------------------------------------------------------------------------

-- Non-monadic data types

-- TODO: split this into TextEditError and LineEditError. The TextEditError should contain a field
-- for a 'LineEditError'.
data TextEditError
  = NoTextInBuffer
  | TextEditError       !StrictBytes
  | EndOfLineBuffer     !RelativeToCursor
  | EndOfCharBuffer     !RelativeToCursor
  | LineIndexOutOfRange !(Absolute LineIndex)
  | LineCountOutOfRange !(Absolute LineIndex) !(Relative LineIndex)
  | CharIndexOutOfRange !(Absolute CharIndex)
  | CharCountOutOfRange !(Absolute CharIndex) !(Relative CharIndex)
  deriving (Eq, Ord, Show)

bufferToEditorError
  :: (Bounded i, IsIndex i, IsLength len, MonadEditVec vec m)
  => (RelativeToCursor -> TextEditError) -- ^ EndOfBuffer
  -> (i -> TextEditError) -- ^ IndexOutOfRnage
  -> (i -> len -> TextEditError) -- ^ CountOutOfRnage
  -> BufferError -> m TextEditError
bufferToEditorError endOfBuffer indexOutOfRange countOutOfRange = \ case
  BufferIsEmpty           -> pure NoTextInBuffer
  UndefinedAtIndex  i     -> error $ "internal error: undefined element at index "++show i
  BufferLimitError  lim   -> pure $ endOfBuffer lim
  BufferIndexBounds i     -> indexOutOfRange <$> indexToAbsolute i
  BufferIndexRange  i len ->
    flip countOutOfRange (toLength $ fromLength len) <$> indexToAbsolute i

bufferToTextEditError :: MonadEditVec vec m => BufferError -> m TextEditError
bufferToTextEditError = bufferToEditorError EndOfLineBuffer LineIndexOutOfRange LineCountOutOfRange

bufferToLineEditError :: MonadEditVec vec m => BufferError -> m TextEditError
bufferToLineEditError = bufferToEditorError EndOfCharBuffer CharIndexOutOfRange CharCountOutOfRange

liftMutableGapBuffer
  :: (Monad m, Monad lifted, Monad prim,
      MonadEditVec vec (GapBuffer vec prim)
     )
  => Lens' st (GapBufferState vec)
  -> (BufferError -> GapBuffer vec prim TextEditError)
  -> (forall any . prim any -> lifted any)
  -> (ExceptT TextEditError (StateT st lifted) a -> m a)
  -> GapBuffer vec prim a
  -> m a
liftMutableGapBuffer gapBufferState convertErr liftPrim constr f = constr $ ExceptT $ do
  (ret, gbst) <- use gapBufferState >>= lift . liftPrim . runGapBuffer
    (catchError (Right <$> f) $ fmap Left . convertErr)
  gapBufferState .= gbst
  case ret of
    Right ret -> return ret
    Left{}    -> error $ "internal error: 'bufferTextToEditorError' evlauated to 'throwError'"

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
--
-- __NOTE:__
-- 
-- * Use the 'unpack' function to convert a 'TextLine' back to it's 'String' value.
--
-- * Evaluating 'intSize' on a 'TextLine' returns the number of actual 'Char' values stored within
--   it, so 'textLineBreakSymbol' is a symbol representing two characters (e.g. "\n\r"), 'intSize'
--   will appear yield one character more than you might expect -- an empty line with only a line
--   break will return an 'intSize' of 2. Use 'textLineCursorSpan' to get the number of logical
--   character values, in which a line-break is considered to be a single unit character.
--
data TextLine tags
  = TextLineUndefined
  | TextLine
    { theTextLineString      :: !CharVector
    , theTextLineBreakSymbol :: !LineBreakSymbol
    , theTextLineTags        :: !tags
    }
  deriving Functor

instance IntSized (TextLine tags) where
  intSize = \ case
    TextLineUndefined               -> 0
    TextLine{theTextLineString=str,theTextLineBreakSymbol=sym} ->
      let len = UVec.length str in case sym of
        NoLineBreak   -> len
        LineBreakNL   -> len
        LineBreakCR   -> len
        LineBreakNUL  -> len
        LineBreakNLCR -> len + 1
        LineBreakCRNL -> len + 1

instance Unpackable (TextLine tags) where
  unpack = \ case
    TextLineUndefined               -> ""
    TextLine{theTextLineString=str,theTextLineBreakSymbol=sym} ->
      UVec.toList (UVec.slice 0 (UVec.length str - 1) str) ++ show sym

instance Show tags => Show (TextLine tags) where
  show = showTextLine show

-- The null-terminated, UTF-8 encoded string of bytes stored in this line of text.
textLineString :: Lens' (TextLine tags) CharVector
textLineString = lens theTextLineString $ \ a b -> a{ theTextLineString = b }

-- | Arbitrary information stored with this text. Typcial use cases for this field may include
-- syntax coloring rules, structured data parsed from the 'textLineString', text search indicies, a
-- diff of changes made, or all of the above.
textLineTags :: Lens' (TextLine tags) tags
textLineTags = lens theTextLineTags $ \ a b -> a{ theTextLineTags = b }

showTextLine :: (tags -> String) -> TextLine tags -> String
showTextLine showTags = \ case
  TextLineUndefined -> "(null)"
  TextLine{theTextLineString=vec,theTextLineTags=tags,theTextLineBreakSymbol=lbrksym} ->
    '(' : show (unpack vec) ++ ' ' : show (show lbrksym) ++
    (let tagstr = showTags tags in if null tagstr then "" else ' ' : tagstr) ++ ")"

-- not for export
textLineBreakSymbol :: Lens' (TextLine tags) LineBreakSymbol
textLineBreakSymbol = lens theTextLineBreakSymbol $ \ a b -> a{ theTextLineBreakSymbol = b }

-- | Like 'unpack', but removes the terminating line-breaking characters.
textLineChomp :: TextLine tags -> String
textLineChomp = \ case
  TextLineUndefined -> ""
  TextLine{theTextLineString=vec} -> UVec.toList $ UVec.slice 0 (UVec.length vec - 1) vec

-- | The empty 'TextLine' value.
emptyTextLine :: tags -> TextLine tags
emptyTextLine tags = TextLine
  { theTextLineString      = UVec.empty
  , theTextLineTags        = tags
  , theTextLineBreakSymbol = NoLineBreak
  }

-- | Evaluates to 'True' if the 'TextLine' is empty. Undefined lines also evaluate to 'True'. (Use
-- 'textLineIsUndefined' to test if the line is undefined.) If the line is empty and not undefined,
-- the given predicate is evaluated on the 'textLineTags' value, and this predicate's result
-- determines whether the line is null.
nullTextLine :: (tags -> Bool) -> TextLine tags -> Bool
nullTextLine nullTags = \ case
  TextLineUndefined -> True
  line              -> UVec.null (theTextLineString line)
    && theTextLineBreakSymbol line == NoLineBreak
    && nullTags (theTextLineTags line)

-- | Evaluates to True if the 'TextLine' is undefined. An undefined 'TextLine' is different from an
-- empty string, it is similar to the 'Prelude.Nothing' constructor.
textLineIsUndefined :: TextLine tags -> Bool
textLineIsUndefined = \ case { TextLineUndefined -> True; _ -> False; }

-- | The 'TextCursorSpan' of a 'TextLine' is essentially the number of steps it would take for an
-- end-user to step the cursor over every element in the 'TextLine', including an optional line
-- break at the end which is computed as a single step regardless of how many UTF characters
-- actually comprise the line break. Most of the time 'textLineCursorSpan' is exactly equal to the
-- vector length of 'theTextLineString', but not always.
--
-- In some cases, a 'TextLine' has multiple line break characters, especially on the Windows
-- operating system in which a line break contains two characters: @"\\n\\r"@. However when using a
-- text editor on such an operating system, pressing the backspace key on the keyboard when at the
-- start of the line will wrap the deletion operation to the previous line, deleting all newline
-- characters in a single key press. Thus the two characters @"\\n\\r"@ have a cursor span of 1 key
-- press.
--
-- This text editor engine accommodates arbitrary line break character sequences by providing this
-- 'textLineCursorSpan' metric, which always treats all line breaking characters as a single unit. If
-- there are no line breaking characters, or if there is only one line breaking character, the
-- 'textLineCursorSpan' is identical to the value given by 'intSize'.
textLineCursorSpan :: TextLine tags -> TextCursorSpan
textLineCursorSpan = TextCursorSpan . \ case
  TextLineUndefined -> error "textLineCursorSpan: undefined line"
  TextLine{theTextLineString=vec} -> UVec.length vec

-- | Return index of the top-most non-line-breaking character. The size of the string not including
-- the line breaking characters is equal to the result of this function evaluated by 'charToIndex'.
textLineTop :: TextLine tags -> Absolute CharIndex
textLineTop = toIndex . subtract 1 . intSize

-- | Get a character at the @('Absolute' 'CharIndex')@ of a 'TextLine'. This may read into the line
-- breaking character without evaluating to 'Nothing'.
textLineGetChar :: TextLine tags -> Absolute CharIndex -> Maybe Char
textLineGetChar txt i0 = do
  let i = fromIndex i0
  guard (i < intSize txt && 0 <= i)
  pure (textLineGetCharNoChk txt i0)

textLineGetCharNoChk :: TextLine tags -> Absolute CharIndex -> Char
textLineGetCharNoChk txt = ((theTextLineString txt) UVec.!) . fromIndex

----------------------------------------------------------------------------------------------------

-- | This data type keeps track of a cursor state within the 'ViewLine' monad.
data ViewLineState tags
  = ViewLineState
    { theViewerCharsBeforeCursor :: VecLength
    , theViewerLine              :: TextLine tags
    }

viewerElemsBeforeCursor :: Lens' (ViewLineState tags) VecLength
viewerElemsBeforeCursor =
  lens theViewerCharsBeforeCursor $ \ a b -> a{ theViewerCharsBeforeCursor = b }

--viewerMirrorCursor :: ViewLineState tags -> Iso' VecLength VecLength
--viewerMirrorCursor view = iso (len -) (len -) where
--  str = view ^. viewerLine . textLineString
--  len = VecLength $ intSize str

--viewerElemsAfterCursor :: Lens' (ViewLineState tags) VecLength
--viewerElemsAfterCursor = lens
--  (\ view -> view ^. viewerElemsBeforeCursor . viewerMirrorCursor view)
--  (\ view i -> view & viewerElemsBeforeCursor . viewerMirrorCursor view .~ i)

viewerLine :: Lens' (ViewLineState tags) (TextLine tags)
viewerLine = lens theViewerLine $ \ a b -> a{ theViewerLine = b }

----------------------------------------------------------------------------------------------------

-- | This is a reference to the stateful data of your buffer of text. The type variable @mvar@ is
-- typically set to 'TextIORef' (a synonym of @'MutVar' IO@), 'TextSTRef' (a synonym of @'MutVar'
-- (ST s)@) or 'TextMutex' (a synonym of @'MVar' IO@). The type variable @m@ is the primitive monad
-- type (usually either @IO@ or @ST@) that depends on the @mvar@ related to @m@ by the
-- 'Control.Primitive.PrimState' type family.
--
-- You edit this text by evaluating any text editing combinators that evaluate to
-- an 'EditText' function type. Declare a new 'TextBuffer' using the 'newTextBuffer' function, then
-- pass this 'TextBuffer' to the 'runEditText' function along with some combinator functions of type
-- @('EditText' tags)@ or type @'MonadEditText' editor => (editor tags)@.
--
-- @
-- main :: IO ()
-- main = do
--     buf <- 'newTextBuffer' ()
--     'runEditText' $ do
--         'insertString' "Hello, world!\\nThis is a text document.\\n"
--         'gotoLocation' 1 0 'Control.Monad.>>' 'insertChar' \'"\'
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
newtype TextBuffer mvar tags = TextBuffer (mvar (TextBufferState mvar tags))

-- | An non-mutex (non-thread-safe) reference that can be mutated in the IO monad.
type TextIORef = MutVar (PrimState IO)

-- | An mutex (thread-safe) reference that can be mutated in the IO monad.
type TextMutex = MVar (PrimState IO)

-- | An non-mutex (non-thread-safe) reference that can be mutated in the ST monad, although the ST
-- monad is almost always a single-threaded computation, so it doesn't matter if it is not
-- thread-safe.
type TextSTRef s = MutVar (PrimState (ST s))

type family VarPrimState (var :: * -> *)
type instance VarPrimState (MutVar primst) = primst
type instance VarPrimState (MVar primst) = primst

class
  (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m
  ) =>
  ModifyReference (mvar :: * -> * -> *) m where
    newReference :: a -> m (mvar (PrimState m) a)
    modifyReference :: mvar (PrimState m) a -> (a -> m (a, b)) -> m b
    readReference :: mvar (PrimState m) a -> m a
    swapReference :: mvar (PrimState m) a -> a -> m a

instance
  (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m
  ) =>
  ModifyReference MutVar m where
    newReference  = newMutVar
    modifyReference ref f = readMutVar ref >>= f >>= \ (a, b) -> writeMutVar ref a >> return b
    readReference ref = readMutVar ref
    swapReference ref a = readMutVar ref <* writeMutVar ref a

instance ModifyReference MVar IO where
  newReference = newMVar
  modifyReference ref f = bracketOnError (takeMVar ref) (putMVar ref)
    (f >=> \ (a, b) -> putMVar ref a >> return b)
  readReference ref = readMVar ref
  swapReference ref b =
    bracketOnError (takeMVar ref) (putMVar ref) (\ a -> putMVar ref b >> return a)

modifyReference_
  :: ModifyReference mvar m
  => mvar (PrimState m) a -> (a -> m a) -> m ()
modifyReference_ mvar = modifyReference mvar . fmap (fmap (flip (,) ()))

----------------------------------------------------------------------------------------------------

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
data TextBufferState mvar tags
  = TextBufferState
    { theBufferDefaultLine   :: !(TextLine tags)
      -- ^ The tag value to use when new 'TextLine's are automatically constructed after a line
      -- break character is inserted.
    , theBufferLineBreaker   :: LineBreaker
      -- ^ The function used to break strings into lines. This function is called every time a
      -- string is transferred from 'theBufferLineEditor' to to 'theLinesAbove' or 'theLinesBelow'.
    , theTextGapBufferState  :: !(GapBufferState (MVec.MVector (VarPrimState mvar) (TextLine tags)))
      -- ^ Contains the gap buffer itself, which is defined in the "Hakshell.GapBuffer" module.
    , theBufferLineEditor    :: !(LineEditor mvar tags)
      -- ^ A data structure for editing individual characters in a line of text.
    , theBufferTargetCol     :: !(Absolute CharIndex)
      -- ^ When moving the cursor up and down the 'TextBuffer' (e.g. using 'gotoLocation'), the
      -- cursor should generally remain at the same character column location.
    }
  -- TODO: First do the TODO item on the 'TextBuffer' data type, then rename this to
  -- 'TextBuffer'. Create a 'runEditText' API function and export it.

-- Not for export: this is only used to set empty lines in the buffer.
bufferDefaultLine :: Lens' (TextBufferState mvar tags) (TextLine tags)
bufferDefaultLine = lens theBufferDefaultLine $ \ a b -> a{ theBufferDefaultLine = b }

-- | Entering a line-breaking character (e.g. @'\n'@) into a 'TextBufferState' using 'insertChar' or
-- 'insertString' results in several 'TextLine's being generated automatically. Whenever a
-- 'TextLine' is constructed, there needs to be a default tag value that is assigned to it. This
-- lens allows you to observe or set the default tag value.
bufferDefaultTags :: Lens' (TextBufferState mvar tags) tags
bufferDefaultTags = bufferDefaultLine . textLineTags

-- Not for export: access to the 'GapBufferState' within the 'TextBufferState' data structure.
textEditGapBuffer
  :: Lens' (TextBufferState mvar tags)
           (GapBufferState (MVec.MVector (VarPrimState mvar) (TextLine tags)))
textEditGapBuffer = lens theTextGapBufferState $ \ a b -> a{ theTextGapBufferState = b }

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
bufferLineBreaker :: Lens' (TextBufferState mvar tags) LineBreaker
bufferLineBreaker = lens theBufferLineBreaker $ \ a b -> a{ theBufferLineBreaker = b }

-- Not for export: indicates whether the line under the cursor is defined or undefined.
lineCursorIsDefined :: Lens' (TextBufferState mvar tags) Bool
lineCursorIsDefined = textEditGapBuffer . gapBufferCursorIsDefined

-- | The current line of text being edited under the cursor.
bufferLineEditor :: Lens' (TextBufferState mvar tags) (LineEditor mvar tags)
bufferLineEditor = lens theBufferLineEditor $ \ a b -> a{ theBufferLineEditor = b }

-- | When moving the cursor up and down the 'TextBuffer' (e.g. using 'gotoLocation'), the cursor
-- should generally remain at the same character column location.
bufferTargetCol :: Lens' (TextBufferState mvar tags) (Absolute CharIndex)
bufferTargetCol = lens theBufferTargetCol $ \ a b -> a{ theBufferTargetCol = b }

----------------------------------------------------------------------------------------------------

-- | The current line that is being edited.
--
-- A line editor needs to have it's characters flushed to it's parent 'TextBuffer' by the
-- 'flushLineEditor' in order for the changes in the 'LineEditor' to be applied to the document in
-- the 'TextBuffer'. The content of a 'LineEditor' can be reverted to the content of the line
-- currently under the cursor by calling 'refillLineEditor'.
--
-- Some cursor motion functions, like 'moveCursor' and 'gotoLocation' automatically evaluate
-- 'flushLineEditor' before moving the cursor and automatically evaluate 'refillLineEditor' after
-- moving the cursor. Other cursor motion functions, like 'gotoLine' and 'moveByLine' __DO_NOT__
-- evaluate 'flushLineEditor' or 'refillLineEditor' at all, allowing you to accumulate 'TextLine's
-- into the 'LineEditor' as you move it the cursor different lines in the 'TextBuffer'.
data LineEditor mvar tags
  = LineEditor
    { theParentTextEditor     :: TextBuffer mvar tags
      -- ^ A line editor can be removed from it's parent 'TextEditor'. A 'LineEditor' is defined
      -- such that it can only directly edit text in it's parent 'TextEditor'. A 'LineEditor' can
      -- indirectly edit text in any other parent, but only if a complete copy of the content of the
      -- line editor is made and passed it to some other 'TextEditor'.
    , theLineEditGapBuffer   :: !(GapBufferState (UMVec.MVector (VarPrimState mvar) Char))
      -- ^ The internal 'GapBufferState'.
    , theLineEditorIsClean   :: !Bool
      -- ^ This is set to 'True' if 'theBufferLineEditor' is a perfect copy of the 'TextLine'
      -- currently being referred to by the 'getLineNumber'. If the content of
      -- 'theBufferLineEditor' has been modified by 'insertChar' or 'deleteChars', or if cursor has
      -- been moved to a different line, this field is set to 'False'.
    , theCursorBreakSymbol   :: !LineBreakSymbol
    , theLineEditorTags      :: tags
    }

instance IntSized (LineEditor mvar tags) where { intSize = lineEditorCharCount; }

parentTextEditor :: Lens' (LineEditor mvar tags) (TextBuffer mvar tags)
parentTextEditor = lens theParentTextEditor $ \ a b -> a{ theParentTextEditor = b }

lineEditGapBuffer
  :: Lens' (LineEditor mvar tags)
           (GapBufferState (UMVec.MVector (VarPrimState mvar) Char))
lineEditGapBuffer = lens theLineEditGapBuffer $ \ a b -> a{ theLineEditGapBuffer = b }

-- Not for export: this gets updated when inserting or deleting characters before the cursor.
charsBeforeCursor :: Lens' (LineEditor mvar tags) VecLength
charsBeforeCursor = lineEditGapBuffer . gapBufferBeforeCursor

-- Not for export: this gets updated when inserting or deleting characters after the cursor.
charsAfterCursor :: Lens' (LineEditor mvar tags) VecLength
charsAfterCursor = lineEditGapBuffer . gapBufferAfterCursor

-- Not for export: controls line break information within the cursor. If this value is zero, it
-- indicates that no line breaking characters have been entered into the cursor yet. If this value
-- is non-zero, it indicates that the line breaking characters do exist after the cursor at some
-- point, and if additional line breaks are inserted the characters after the cursor need to be
-- split off into a new 'TextLine'.
cursorBreakSymbol :: Lens' (LineEditor mvar tags) LineBreakSymbol
cursorBreakSymbol = lens theCursorBreakSymbol $ \ a b -> a{ theCursorBreakSymbol = b }

-- | A 'Control.Lens.Lens' to get or set tags for the line currently under the cursor. To use or
-- modify the tags value of the line under the cursor, evaluate one of the functions 'use',
-- 'modifying', @('Control.Lens..=')@, or @('Control.Lens.%=')@ within an 'EditText' function, or
-- any function which instantiates 'MonadEditText'.
lineEditorTags :: Lens' (LineEditor mvar tags) tags
lineEditorTags = lens theLineEditorTags $ \ a b -> a{ theLineEditorTags = b }

-- | This is set to 'True' if 'theBufferLineEditor' is a perfect copy of the 'TextLine' currently
-- being referred to by the 'getLineNumber'. If the content of 'theBufferLineEditor' has been
-- modified by 'insertChar' or 'deleteChars', or if cursor has been moved to a different line, this
-- field is set to 'False'.
lineEditorIsClean :: Lens' (LineEditor mvar tags) Bool
lineEditorIsClean = lens theLineEditorIsClean $ \ a b -> a{ theLineEditorIsClean = b }

-- This is set to 'True' if the element under the cursor is defined, 'False' if the element under
-- the cursor is undefined.
charCursorIsDefined :: Lens' (LineEditor mvar tags) Bool
charCursorIsDefined = lineEditGapBuffer . gapBufferCursorIsDefined

----------------------------------------------------------------------------------------------------

instance (PrimMonad m, PrimState m ~ st) =>
  MonadGapBuffer
    (MVec.MVector st (TextLine tags))
    (GapBuffer (MVec.MVector st (TextLine tags)) m)
  where
    type WriteVecElem (MVec.MVector st (TextLine tags)) = TextLine tags
    nullElem = pure TextLineUndefined
    newVector (VecLength siz) = nullElem >>= lift . MVec.replicate siz
    setCursorIsDefined = assign gapBufferCursorIsDefined

-- | This is a type of functions that can modify the textual content stored in a 'TextBufferState'.
newtype EditText mvar tags m a
  = EditText (
      ReaderT (TextBuffer mvar tags)
        (ExceptT TextEditError (StateT (TextBufferState mvar tags) m)) a
    )
  deriving
    ( Functor, Applicative, Monad,
      MonadError TextEditError
    )

--instance Monad m => MonadState (TextBufferState tags) (EditText tags m) where
--  state = EditText . lift . state
--
--instance Monad m => MonadError TextEditError (EditText tags m) where
--  throwError = EditText . throwError
--  catchError (EditText try) catch = EditText $ catchError try $ unwrapEditText . catch

instance MonadTrans (EditText mvar tags) where
  lift = EditText . lift . lift . lift

instance (Monad m, Semigroup a) => Semigroup (EditText mvar tags m a) where
  a <> b = (<>) <$> a <*> b

instance (Monad m, Monoid a) => Monoid (EditText mvar tags m a) where
  mempty = return mempty
  mappend a b = mappend <$> a <*> b

editTextState
  :: PrimMonad m
  => StateT (TextBufferState mvar tags) m a
  -> EditText mvar tags m a
editTextState = EditText . lift . lift

-- | Evaluate an 'EditText' function on the given 'TextBufferState'. The given 'EditText' function
-- is evaluates all updates on the given 'TextBuffer' atomically and in-place (i.e. without copying
-- anything unless instructed to do so). This is the most efficient way to evaluate an 'EditText'
-- function, but is more restrictive in that it can only be evaluated when the 'EditText' data
-- type's @m@ parameter is set to the @IO@ type, meaning you cannot use this function if the
-- 'EditText's @m@ parameter is something other than @IO@, like for example a 'ReaderT' type.
runEditText
  :: ModifyReference mvar m
  => EditText (mvar (PrimState m)) tags m a
  -> TextBuffer (mvar (PrimState m)) tags
  -> m (Either TextEditError a)
runEditText (EditText f) this@(TextBuffer mvar) = modifyReference mvar $
  fmap (\ (a,b) -> (b,a)) . runStateT (runExceptT $ runReaderT f this)

-- | Evaluate an 'EditText' function on the given 'TextBufferState', but unlike 'runEditTextIO', a
-- copy of the entire text buffer is first created, and all updates are performed on the copy
-- atomically. This function is less restrictive than 'runEditTextIO' because it will work for
-- 'EditText' functions where the @m@ parameter is not just @IO@ but any member of the 'Monad'
-- typeclass, however the trade-off is that a copy of the text buffer must be made.
runEditTextOnCopy
  :: ModifyReference mvar m
  => EditText (mvar (PrimState m)) tags m a
  -> TextBuffer (mvar (PrimState m)) tags
  -> m (Either TextEditError a, TextBuffer (mvar (PrimState m)) tags)
runEditTextOnCopy (EditText f) = copyTextBuffer >=> \ copy@(TextBuffer mvar) -> do
  (result, st) <- readReference mvar >>= runStateT (runExceptT $ runReaderT f copy)
  void $ swapReference mvar st
  return (result, copy)

-- Extracts 'textEditGapBuffer', uses it to evaluate 'runGapBuffer', then after the 'runGapBuffer'
-- evaluation completes (which may have updated the 'GapBuffer'), replaces the updated
-- 'textEditGapBuffer'.
editTextLiftGapBuffer
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => GapBuffer (MVec.MVector (PrimState m) (TextLine tags)) m a
  -> EditText (mvar (PrimState m)) tags m a
editTextLiftGapBuffer =
  liftMutableGapBuffer textEditGapBuffer bufferToTextEditError id (EditText . lift)

-- | Like 'textLineIndex', but throws a 'LineIndexOutOfRange' exception if the given 'LineIndex' is
-- out of bounds.
validateLineIndex
  :: ModifyReference mvar m
  => Absolute LineIndex
  -> EditText (mvar (PrimState m)) tags m (Absolute LineIndex)
validateLineIndex = editTextLiftGapBuffer . (validateIndex >=> indexToAbsolute)

-- | Like 'validateLineIndex', but rather than throwing an exception, simply returns 'False' if an
-- exception would have been thrown, and returns 'True' otherwise.
lineIndexIsValid
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => Absolute LineIndex -> EditText (mvar (PrimState m)) tags m Bool
lineIndexIsValid = editTextLiftGapBuffer . fmap fst . testIndex

-- | Evaluates 'validateLineIndex' on a given 'LineIndex', and if no exceptions are thrown, also
-- dereferences the index, returning the 'TextLine' stored at the given normalized logical index.
validateGetLineIndex
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => Absolute LineIndex
  -> EditText (mvar (PrimState m)) tags m (Absolute LineIndex, TextLine tags)
validateGetLineIndex = editTextLiftGapBuffer .
  (validateIndex >=> \ i -> (,) <$> indexToAbsolute i <*> getElemIndex i)

----------------------------------------------------------------------------------------------------

-- | Functions of this type operate on a single 'TextLine', it is useful for updating the
-- 'bufferLineEditor'.
newtype EditLine mvar tags m a
  = EditLine (ExceptT TextEditError (StateT (LineEditor mvar tags) (EditText mvar tags m)) a)
  deriving
    ( Functor, Applicative, Monad,
      MonadError TextEditError
    )
  -- TODO: try lifting @m@ instead of @(EditText tags m)@

--instance Monad m => MonadState (LineEditor mvar tags) (EditLine mvar tags m) where
--  state = EditLine . lift . state
--
--instance Monad m =>  MonadError TextEditError (EditLine mvar tags m) where
--  throwError = EditLine . throwError
--  catchError (EditLine try) catch = EditLine $ catchError try $ unwrapEditLine . catch

instance MonadTrans (EditLine mvar tags) where
  lift = EditLine . lift . lift . lift

editLineState
  :: PrimMonad m
  => StateT (LineEditor mvar tags) (EditText mvar tags m) a
  -> EditLine mvar tags m a
editLineState = EditLine . lift

-- Extracts 'textEditGapBuffer', uses it to evaluate 'runGapBuffer', then after the 'runGapBuffer'
-- evaluation completes (which may have updated the 'GapBuffer'), replaces the updated
-- 'textEditGapBuffer'.
editLineLiftGapBuffer
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => GapBuffer (UMVec.MVector (PrimState m) Char) m a
  -> EditLine (mvar (PrimState m)) tags m a
editLineLiftGapBuffer =
  liftMutableGapBuffer lineEditGapBuffer bufferToLineEditError lift EditLine

-- | Perform an edit on the line under the cursor. It is usually not necessary to invoke this
-- function directly, the definition of 'liftEditLine' for the 'TextEdit' function type is this
-- function, so any function that evaluates to an @editor@ where the @editor@ is a member of the
-- 'MonadEditLine' typeclass will automatically invoke this function based on the function type of
-- the context in which it is used.
editLine :: PrimMonad m => EditLine mvar tags m a -> EditText mvar tags m a
editLine (EditLine f) = editTextState (use bufferLineEditor) >>=
  runStateT (runExceptT f) >>= \ case
    (Left err, _   ) -> throwError err
    (Right  a, line) -> editTextState (bufferLineEditor .= line) >> return a

----------------------------------------------------------------------------------------------------

-- | This is a read-only folding function, unlikes 'FoldMapLines' which can perform updates to the
-- text in the buffer. 'FoldLines' functions will never change the position of the text cursor.
--
-- To run 'FoldLines' function, use one of 'foldLines', 'foldAllLines', 'foldLinesBetween', or
-- 'foldLinesInRange'. The @editor@ type variable here can be one of 'EditText' or 'ViewText', or
-- really any function type that instantiates the 'TextFoldable' typeclass.
newtype FoldLines r fold (editor :: * -> *) a
  = FoldLines
    { unwrapFoldLines :: ContT r (ExceptT TextEditError (StateT fold editor)) a }
  deriving (Functor, Applicative, Monad, MonadIO)

type FoldLinesHalt fold editor void = FoldLines () fold editor void

instance Monad editor => MonadState fold (FoldLines r fold editor) where
  state = FoldLines . lift . lift . state

instance Monad editor => MonadError TextEditError (FoldLines r fold editor) where
  throwError = FoldLines . lift . throwError
  catchError (FoldLines try) catch =
    FoldLines $ ContT $ \ next ->
    catchError (runContT try next) $ \ err ->
    runContT (unwrapFoldLines $ catch err) next

--instance MonadTrans editor => MonadTrans (FoldLines r fold editor) where
--  lift = foldLinesLift . lift

instance Monad editor => MonadCont (FoldLines r fold editor) where
  callCC f = FoldLines $ callCC $ unwrapFoldLines . f . fmap FoldLines

instance (Monad editor, Semigroup a) => Semigroup (FoldLines r fold editor a) where
  a <> b = (<>) <$> a <*> b

instance (Monad editor, Monoid a) => Monoid (FoldLines r fold editor a) where
  mappend a b = mappend <$> a <*> b
  mempty = return mempty

class Monad editor => TextFoldable editor where
  -- | Evaluate a 'FoldLines' function between two @('Absolute' 'LineIndex')@ markers, including the
  -- given line indicies.
  foldLinesBetween
    :: (Monad editor, EditorTagsType editor ~ tags)
    => Absolute LineIndex
    -> Absolute LineIndex
    -> fold
    -> (FoldLines () fold editor void ->
        Absolute LineIndex -> TextLine tags -> FoldLines () fold editor ()
       )
    -> editor fold

instance
  (PrimMonad m, primst ~ PrimState m, VarPrimState (mvar (PrimState m)) ~ primst,
   EditorTagsType (EditText (mvar primst) tags m) ~ tags
  ) => TextFoldable (EditText (mvar primst) tags m) where
    foldLinesBetween = gFoldMapBetween
      (foldLinesLift . lift)
      editTextLiftGapBuffer
      runFoldLinesStep
      (readSlice MVec.read)
      (\ _ _ -> return)
      (sliceSize MVec.length)

-- not for export
--
-- We don't want users to lift arbitrary editor commands while a fold is being performed, especially
-- 'pushLine' or 'popLine', which will result in the fold loop function, which determines which
-- array indicies to read before the loop begins and does not alter these indicies while looping,
-- will read an index that was 'popLine'd and pass an 'TextLineUndefined' value to the fold function.
foldLinesLift :: Monad editor => editor a -> FoldLines r fold editor a
foldLinesLift = FoldLines . lift . lift . lift

-- | Evaluate a 'FoldLines' function to a 'EditText' function without performing any looping.
runFoldLinesStep
  :: MonadError TextEditError editor
  => FoldLines a fold editor a -> fold -> editor (a, fold)
runFoldLinesStep (FoldLines f) = runStateT (runExceptT $ runContT f return) >=> \ case
  (Left err, _   ) -> throwError err
  (Right  a, fold) -> return (a, fold)

-- not for export
--
-- This function is used to define the algorithm used by several functions. Here is a list of return
-- types followed by the functions that return them.
gFoldMapBetween ::
  ( MonadEditVec (vec (TextLine tags)) (gapperM m),
    PrimMonad m, MonadError BufferError (gapperM m),
    MonadCont loopM, Monad editor
  )
  => (forall any . m any -> loopM any)
  -> (forall any . gapperM m any -> editor any)
  -> (loopM () -> fold -> editor ((), fold))
  -> (UnsafeSlice (vec (TextLine tags)) -> VecIndex -> m (TextLine tags))
  -> (UnsafeSlice (vec (TextLine tags)) -> VecIndex -> putBack -> m ())
  -> (UnsafeSlice (vec (TextLine tags)) -> VecLength)
  -> Absolute LineIndex
  -> Absolute LineIndex
  -> fold
  -> (loopM void -> Absolute LineIndex -> TextLine tags -> loopM putBack)
  -> editor fold
gFoldMapBetween liftM liftGapper runStep readVec writeVec sliceSize from0 to0 st fold =
  join $ liftGapper $ do
    let reverseFold = from0 > to0
    from  <- validateIndex   from0
    to    <- validateIndex   to0
    from0 <- indexToAbsolute from
    to0   <- indexToAbsolute to
    let foldElemLoop halt vec = \ case
          []              -> return ()
          (i, linum):next -> liftM (readVec vec i) >>= \ elem ->
            fold halt linum elem >>= liftM . writeVec vec i >>
            foldElemLoop halt vec next
    let foldElem halt vec linum = foldElemLoop halt vec $
          if reverseFold
          then zip (sliceIndiciesReverse sliceSize vec) (iterate (subtract 1) $ linum - 1)
          else zip (sliceIndiciesForward sliceSize vec) (iterate (+ 1) linum)
    withRegion from to $ \ lo hi -> return $ fmap snd $
      flip runStep st $ callCC $ \ halt ->
      if reverseFold then do
        foldElem (halt ()) hi to0
        foldElem (halt ()) lo $ shiftAbsolute to0 $ negate $
          toLength $ fromLength $ sliceSize hi
      else do
        foldElem (halt ()) lo from0
        foldElem (halt ()) hi $ shiftAbsolute from0 $
          toLength $ fromLength $ sliceSize lo

-- | Evaluates 'foldLinesBetween' but takes a @'Relative' 'LineIndex'@ as the bounding parameter,
-- which is relative to the starting @'Absolute' 'LineIndex'@.
foldLinesInRange
  :: (TextFoldable editor, EditorTagsType editor ~ tags)
  => Absolute LineIndex
  -> Relative LineIndex
  -> fold
  -> (FoldLinesHalt fold editor void
      -> Absolute LineIndex
      -> TextLine tags
      -> FoldLines () fold editor ()
     )
  -> editor fold
foldLinesInRange from rel fold f = if rel == 0 then return fold else
  foldLinesBetween from (shiftAbsolute from rel) fold f

-- | Evaluates 'foldLinesBetween' but takes a @'Relative' 'LineIndex'@ as the bounding parameter,
-- which is relative to current cursor index.
foldLines
  :: (TextFoldable editor, CursorIndexedText editor, EditorTagsType editor ~ tags)
  => Relative LineIndex
  -> fold
  -> (FoldLinesHalt fold editor void
      -> Absolute LineIndex
      -> TextLine tags
      -> FoldLines () fold editor ()
     )
  -> editor fold
foldLines rel fold f = getLineNumber >>= \ from -> foldLinesInRange from rel fold f

-- | Evaluates 'foldLinesBetween' but using the first and last line in the current buffer as the
-- bounding parameters.
foldAllLines
  :: (TextFoldable editor, EditorTagsType editor ~ tags)
  => fold
  -> (FoldLinesHalt fold editor void
      -> Absolute LineIndex
      -> TextLine tags
      -> FoldLines () fold editor ()
     )
  -> editor fold
foldAllLines = foldLinesBetween minBound maxBound

----------------------------------------------------------------------------------------------------

-- | This function updates lines in a 'TextBuffer' using one of the functions 'mapLinesBetween'
-- 'mapLinesInRange', 'mapLines', or 'mapAllLines'. Functions of this type are not able to modify
-- the @tags@ type, but they can make other modifications to each of the 'TextLine's in the
-- 'TextBuffer'.
newtype MapLines mvar tags r (m :: * -> *) a
  = MapLines{ unwrapMapLines :: ContT r (EditText mvar tags m) a}
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadError TextEditError(MapLines mvar tags r m) where
  throwError = MapLines . lift . throwError
  catchError (MapLines try) catch = MapLines $ ContT $ \ next ->
    catchError (runContT try next) $ \ err ->
    runContT (unwrapMapLines $ catch err) next

instance Monad m => MonadCont (MapLines mvar tags r m) where
  callCC f = MapLines $ callCC $ unwrapMapLines . f . fmap MapLines
 
instance (Monad m, Monoid a) => Monoid (MapLines mvar tags r m a) where
  mappend a b = mappend <$> a <*> b
  mempty = return mempty
 
instance (Monad m, Semigroup a) => Semigroup (MapLines mvar tags r m a) where
  (<>) a b = (<>) <$> a <*> b

instance MonadTrans (MapLines mvar tags r) where
  lift = MapLines . lift . lift

runMapLinesStep :: Monad m => MapLines mvar tags a m a -> EditText mvar tags m a
runMapLinesStep (MapLines f) = runContT f return

mapLinesBetween
  :: ModifyReference mvar m
  => Absolute LineIndex
  -> Absolute LineIndex
  -> (MapLines (mvar (PrimState m)) tags () m void
      -> Absolute LineIndex
      -> TextLine tags
      -> MapLines (mvar (PrimState m)) tags () m (TextLine tags)
     )
  -> EditText (mvar (PrimState m)) tags m ()
mapLinesBetween from to = gFoldMapBetween
  lift
  editTextLiftGapBuffer
  (\ f () -> flip (,) () <$> runMapLinesStep f)
  (readSlice MVec.read)
  writeSlice
  (sliceSize MVec.length)
  from
  to
  ()

mapLinesInRange
  :: ModifyReference mvar m
  => Absolute LineIndex
  -> Relative LineIndex
  -> (MapLines (mvar (PrimState m)) tags () m void
      -> Absolute LineIndex
      -> TextLine tags
      -> MapLines (mvar (PrimState m)) tags () m (TextLine tags)
     )
  -> EditText (mvar (PrimState m)) tags m ()
mapLinesInRange from rel f = if rel == 0 then return () else
  mapLinesBetween from (shiftAbsolute from rel) f

mapLines
  :: ModifyReference mvar m
  => Relative LineIndex
  -> (MapLines (mvar (PrimState m)) tags () m void
      -> Absolute LineIndex
      -> TextLine tags
      -> MapLines (mvar (PrimState m)) tags () m (TextLine tags)
     )
  -> EditText (mvar (PrimState m)) tags m ()
mapLines rel f = getLineNumber >>= \ from -> mapLinesInRange from rel f

mapAllLines
  :: ModifyReference mvar m
  => (MapLines (mvar (PrimState m)) tags () m void
      -> Absolute LineIndex
      -> TextLine tags
      -> MapLines (mvar (PrimState m)) tags () m (TextLine tags)
     )
  -> EditText (mvar (PrimState m)) tags m ()
mapAllLines = mapLinesBetween minBound maxBound

----------------------------------------------------------------------------------------------------

-- | A type of functions that can perform a fold (in the sense of the Haskell 'Control.Monad.foldM'
-- function) over lines of text in a 'TextBufferState' while also updating each line as they are
-- being folded. Each line of text is folded in place, and lines may not be removed or inserted (for
-- inserting or removing lines , use 'cursorLoop').
--
-- This function takes an arbitrary @fold@ data type which can be anything you want, and is
-- initialized when evaluating the 'runFoldMapLines' function. The 'FoldMapLines' function type
-- instantiates 'Control.Monad.State.Class.MonadState' over the @fold@ type, so you will use
-- 'Control.Monad.State.state', 'Control.Monad.State.modify', 'Control.Monad.State.get', and
-- 'Control.Monad.State.Put' functions
--
-- Not that the term "fold" as it is used in this function's name is not to be confused with "fold"
-- as in folding paper. "Folding" is a term that many graphical text editors use to describe a
-- feature in which a block of contiguous lines of text can be hidden (not displayed on screen), as
-- if the text buffer were a piece of paper and the paper was folded twice, once above the start of
-- the first line of the block of text, and once below the bottom line of the block of text, then
-- pushing the edges of the folds of paper together to obscure the text between the folds. The
-- 'FoldMapLines' has nothing to do with such a feature.
newtype FoldMapLines mvar tags fold r m a
  = FoldMapLines
    { unwrapFoldMapLines :: ContT r (ExceptT TextEditError (StateT fold (EditText mvar tags m))) a
    }
  deriving (Functor, Applicative, Monad)

type instance EditorTagsType (FoldMapLines mvar tags fold r m) = tags

instance Monad m => MonadState fold (FoldMapLines mvar tags fold r m) where
  state = FoldMapLines . lift . state

instance Monad m => MonadError TextEditError (FoldMapLines mvar tags fold r m) where
  throwError = FoldMapLines . lift . throwError
  catchError (FoldMapLines try) catch = FoldMapLines $ ContT $ \ next ->
    catchError (runContT try next) $ flip runContT next . unwrapFoldMapLines . catch

instance Monad m => MonadCont (FoldMapLines mvar tags fold r m) where
  callCC f = FoldMapLines $ callCC $ unwrapFoldMapLines . f . (FoldMapLines .)

instance MonadTrans (FoldMapLines mvar tags fold r) where
  lift = foldMapLiftEditText . lift

instance (Monad m, Semigroup a) => Semigroup (FoldMapLines mvar tags fold r m a) where
  a <> b = (<>) <$> a <*> b

instance (Monad m, Monoid a) => Monoid (FoldMapLines mvar tags fold r m a) where
  mappend a b = mappend <$> a <*> b
  mempty = return mempty

foldMapLiftEditText :: Monad m => EditText mvar tags m a -> FoldMapLines mvar tags fold r m a
foldMapLiftEditText = FoldMapLines . lift . lift . lift

-- | Convert a 'FoldMapLines' into an 'EditText' function. This function is analogous to the
-- 'runStateT' function. This function does not actually perform a fold or map operation, rather it
-- simply unwraps the 'EditText' monad that exists within the 'FoldMapLines' monad.
runFoldMapLinesStep
  :: Monad m
  => FoldMapLines mvar tags fold r m a
  -> (a -> EditText mvar tags m r)
  -> fold -> EditText mvar tags m (r, fold)
runFoldMapLinesStep (FoldMapLines f) end =
  runStateT (runExceptT $ runContT f $ lift . lift . end) >=> \ case
    (Left err, _   ) -> throwError err
    (Right  a, fold) -> return (a, fold)

foldMapLinesBetween
  :: ModifyReference mvar m
  => Absolute LineIndex
  -> Absolute LineIndex
  -> fold
  -> (FoldMapLines (mvar (PrimState m)) tags fold () m void
      -> Absolute LineIndex
      -> TextLine tags
      -> FoldMapLines (mvar (PrimState m)) tags fold () m (TextLine tags)
     )
  -> EditText (mvar (PrimState m)) tags m fold
foldMapLinesBetween = gFoldMapBetween
  lift
  editTextLiftGapBuffer
  (\ f -> runFoldMapLinesStep f $ const $ return ())
  (readSlice MVec.read)
  writeSlice
  (sliceSize MVec.length)

foldMapLinesInRange
  :: ModifyReference mvar m
  => Absolute LineIndex
  -> Relative LineIndex
  -> fold
  -> (FoldMapLines (mvar (PrimState m)) tags fold () m void
      -> Absolute LineIndex
      -> TextLine tags
      -> FoldMapLines (mvar (PrimState m)) tags fold () m (TextLine tags)
     )
  -> EditText (mvar (PrimState m)) tags m fold
foldMapLinesInRange from rel fold f = if rel == 0 then return fold else
  foldMapLinesBetween from (shiftAbsolute from rel) fold f

foldMapLines
  :: ModifyReference mvar m
  => Relative LineIndex
  -> fold
  -> (FoldMapLines (mvar (PrimState m)) tags fold () m void
      -> Absolute LineIndex
      -> TextLine tags
      -> FoldMapLines (mvar (PrimState m)) tags fold () m (TextLine tags)
     )
  -> EditText (mvar (PrimState m)) tags m fold
foldMapLines rel fold f = getLineNumber >>= \ from -> foldMapLinesInRange from rel fold f

foldMapAllLines
  :: ModifyReference mvar m
  => fold
  -> (FoldMapLines (mvar (PrimState m)) tags fold () m void
      -> Absolute LineIndex
      -> TextLine tags
      -> FoldMapLines (mvar (PrimState m)) tags fold () m (TextLine tags)
     )
  -> EditText (mvar (PrimState m)) tags m fold
foldMapAllLines = foldMapLinesBetween minBound maxBound

----------------------------------------------------------------------------------------------------

-- | This is a monadic function type used to group together functions that inspect, or perform folds
-- over, 'TextLine' values.
newtype ViewLine tags m a
  = ViewLine
    { unwrapViewLine :: ExceptT TextEditError (StateT (ViewLineState tags) m) a }
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadState (ViewLineState tags) (ViewLine tags m) where
  state = ViewLine . state

instance Monad m => MonadError TextEditError (ViewLine tags m) where
  throwError = ViewLine . throwError
  catchError (ViewLine try) catch = ViewLine $ catchError try $ unwrapViewLine . catch

viewLineLiftIIBuffer
  :: Monad m
  => IIBuffer (UVec.Vector Char) m a
  -> ViewLine tags m a
viewLineLiftIIBuffer f = ViewLine $ ExceptT $ do
  vec <- use (viewerLine . textLineString)
  cur <- use viewerElemsBeforeCursor
  (ret, st) <- lift $ flip runIIBuffer
    IIBufferState{ theIIBufferVector = vec, theIIBufferCursor = cur }
    (catchError (Right <$> f) $ fmap Left . bufferToTextEditError)
  viewerLine . textLineString .= st ^. iiBufferVector
  viewerElemsBeforeCursor     .= st ^. iiBufferCursor
  case ret of
    Right a -> return a
    Left{}  -> error $ "internal error: 'bufferTextToEditorError' evaluated to 'throwError'"

liftViewLine :: MonadError TextEditError m => ViewLine tags Identity a -> TextLine tags -> m a
liftViewLine f = (throwError ||| pure) . runViewLine f

-- | Run a 'ViewLine' function in some monadic function context @m@. See also 'runViewLine'.
runViewLineT :: Monad m => ViewLine tags m a -> TextLine tags -> m (Either TextEditError a)
runViewLineT (ViewLine f) = \ case
  TextLineUndefined -> error "internal error: evaluated 'runViewLine' on an undefined line"
  line -> evalStateT (runExceptT f) $
    ViewLineState{ theViewerCharsBeforeCursor = 0, theViewerLine = line }

-- | Like 'runViewLineT' but runs in a pure 'Identity' monad
runViewLine :: ViewLine tags Identity a -> TextLine tags -> Either TextEditError a
runViewLine f = runIdentity . runViewLineT f

-- | Returns the top-most index of the 'TextLine' being viewed. If the 'TextLine' is empty or
-- undefined, 'Nothing' is returned.
viewerTopChar :: Monad m => ViewLine tags m (Maybe (Absolute CharIndex))
viewerTopChar = use viewerLine <&> \ case
  TextLineUndefined -> Nothing
  line              -> Just $ toIndex $ intSize line - 1

cursorIsOnLineBreak :: Monad m => ViewLine tags m Bool
cursorIsOnLineBreak = indexIsOnLineBreak <$> use viewerLine <*> viewLineLiftIIBuffer cursorIndex

-- | Move the cursor of the 'TextFrame' to the given 'Absolute' 'CharIndex'.
viewerCursorToChar :: Monad m => Absolute CharIndex -> ViewLine tags m ()
viewerCursorToChar = viewLineLiftIIBuffer .
  (validateIndex >=> (iiBufferCursor .=) . distanceFromOrigin)

-- Not for export
--
-- Copies the tags and updates the line break symbol
similarLine
  :: Monad m
  => (LineBreakSymbol -> LineBreakSymbol) -> CharVector -> ViewLine tags m (TextLine tags)
similarLine onLbrk vec = do
  line <- use viewerLine
  pure $ (textLineBreakSymbol %~ onLbrk) . (textLineString .~ vec) $ line

-- | Cut and remove a line at some index, keeping the characters 'Before' or 'After' the index.
sliceLineToEnd :: Monad m => RelativeToCursor -> ViewLine tags m (TextLine tags)
sliceLineToEnd = (<$> splitLine) . (\ case { Before -> fst; After  -> snd; })

-- | Splits the current 'TextLine' at the current cursor location.
splitLine :: Monad m => ViewLine tags m (TextLine tags, TextLine tags)
splitLine = do
  (before, after) <- viewLineLiftIIBuffer $ withFullSlices $ \ before after ->
    return (safeClone before, safeClone after)
  (,) <$> similarLine (if UVec.null after then id else const NoLineBreak) before
      <*> similarLine (if UVec.null after then const NoLineBreak else id) after

----------------------------------------------------------------------------------------------------

-- | Tests the validity of the 'lineIndex' of a given 'TextLocation' using 'testIndex', and if
-- the 'lineIndex' is valid, also tests if the 'charIndex' is valid. This function only throws an
-- exception if the 'TextBuffer' is empty. "Extreme" indicies (e.g. 'minBound' or 'maxBound') are
-- also considered to be valid index values, and are mapped to their nearest respective valid
-- 'TextLocation' values, and returned as a valid, non-extreme 'TextLocation' value.
--
-- You should pattern match on the value returned by this function using a case statement. The
-- following patterns indicate the degree to which the given 'TextLocation' was valid:
--
-- @
--  do loc <- 'testLocation' _some_location_
--     case result of
--       (Left loc) ->
--           ... indicates that the 'lineIndex' of the given 'TextLocation' was INVALID, and @loc@
--               is the 'TextLocation' value for the nearest valid 'TextLocation' value...
-- 
--       (Right (textln, valid)) ->
--           ... indicates that the 'lineIndex' of the given 'TextLocation' is VALID, and
--               "textln" is bound to the 'TextLine' at that given 'LineIndex'.
-- 
--           case valid of
--               (False, index) ->
--                   ... indicates that the 'charIndex' of the given 'TextLocation is INVALID,
--                       and "index" is the 'TextLocation' value nearest to the given
--                       invalid 'TextLocation' value.
--
--               (True, index)  ->
--                   ... indicates that the given 'TextLocation' is VALID, and "index" is the
--                       non-extreme valid 'TextLocation' that could be validated.
-- @
testLocation
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => TextLocation
  -> EditText (mvar (PrimState m)) tags m (Either TextLocation (TextLine tags, (Bool, TextLocation)))
testLocation loc = editTextLiftGapBuffer $ do
  (ok, linum) <- testIndex $ loc ^. lineIndex
  idx <- indexToAbsolute linum
  loc <- pure (loc & lineIndex .~ idx)
  if not ok then return $ Left loc else do
    textln <- absoluteIndex linum >>= getElemIndex
    fmap fst $ flip gapBufferLiftIIBuffer
      (IIBufferState
        { theIIBufferVector = textln ^. textLineString
        , theIIBufferCursor = 0
        })
      (do (ok, i) <- testIndex $ loc ^. charIndex
          chnum <- indexToAbsolute i
          return $ Right (textln, (ok, loc & charIndex .~ chnum))
      )

-- | Like 'testLocation', but evaluates to an exception if the 'TextLocation' is invalid.
validateLocation
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => TextLocation -> EditText (mvar (PrimState m)) tags m (TextLocation, TextLine tags)
validateLocation loc0 = testLocation loc0 >>= \ case
  Left{}  -> throwError $ LineIndexOutOfRange $ loc0 ^. lineIndex
  Right (textln, (ok, loc)) -> if ok then return (loc, textln) else
    throwError $ CharIndexOutOfRange $ loc0 ^. charIndex

-- | Like 'validateLocation' but rather than throwing an exception, returns 'Nothing'. If the
-- location is valid, the 'TextLine' at the valid 'TextLocation' is returned as a 'Just' value.
locationIsValid
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => TextLocation
  -> EditText (mvar (PrimState m)) tags m (Maybe (TextLine tags))
locationIsValid = (`catchError` (const $ return Nothing)) . fmap (Just . snd) . validateLocation

-- | This predicate function checks if an @('Absolute' 'CharIndex')@ points to the line breaking
-- character beyond the last character in a 'TextLine'. When single-stepping a 'TextLocation' cursor
-- such that you evaluate 'locationIsValid' after every step, it is important to evaluate this
-- function on the resultant 'TextLine', and if this function evaluates to 'True' you must increment
-- the 'lineIndex' and reset the 'charIndex' to @1@.
indexIsOnLineBreak :: TextLine tags -> VecIndex -> Bool
indexIsOnLineBreak line = ((unwrapTextCursorSpan $ textLineCursorSpan line) ==) . fromIndex

----------------------------------------------------------------------------------------------------

-- | Use this to initialize a new empty 'TextBufferState'. The default 'bufferLineBreaker' is set to
-- 'lineBreakerNLCR'. A 'TextBufferState' always contains one empty line, but a line must have a @tags@
-- tag, so it is necessary to pass an initializing tag value of type @tags@ -- if you need nothing
-- but plain text editing, @tags@ can be unit @()@.
--
-- The first parameter is an initial buffer size. If a buffer fills up, it will re-allocate itself
-- to a larger size. If you expect to do a lot of text editing on this buffer, set a larger size to
-- avoid re-allocations. If you expect to moslty just read through this buffer set a slightly lower
-- size to avoid wasting space.
newTextBuffer
  :: (ModifyReference mvar m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => Maybe VecLength -> tags -> m (TextBuffer (mvar (PrimState m)) tags)
newTextBuffer initSize tags = do
  cur <- UMVec.new defaultInitLineBufferSize >>=
    lineEditorFromVector tags (pure ())
  MVec.replicate (maybe defaultInitTextBufferSize fromLength initSize) TextLineUndefined >>=
    textBufferFromVector tags cur (pure ())

-- not for export
textBufferFromVector
  :: (ModifyReference mvar m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => tags
  -> LineEditor (mvar (PrimState m)) tags
  -> GapBuffer (MVec.MVector (PrimState m) (TextLine tags)) m ()
  -> MVec.MVector (PrimState m) (TextLine tags)
  -> m (TextBuffer (mvar (PrimState m)) tags)
textBufferFromVector tags cur initVec mvec = do
  mvar <- textBufferStateFromVector tags cur initVec mvec >>= newReference
  let this = TextBuffer mvar
  modifyReference_ mvar $ return . (bufferLineEditor . parentTextEditor .~ this)
  return this

-- | Create a deep-copy of a 'TextBuffer'. Everything is copied perfectly, including the cursor
-- location, and the content and state of the cursor.
copyTextBuffer
  :: (ModifyReference mvar m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => TextBuffer (mvar (PrimState m)) tags -> m (TextBuffer (mvar (PrimState m)) tags)
copyTextBuffer (TextBuffer mvar) = modifyReference mvar $ \ oldst -> do
  gbst       <- snd <$> runGapBuffer (cloneGapBufferState >>= put) (oldst ^. textEditGapBuffer)
  newLineBuf <- copyLineEditor (oldst ^. bufferLineEditor)
  mvar       <- newReference oldst
    { theBufferLineEditor   = newLineBuf
    , theTextGapBufferState = gbst
    }
  let this = TextBuffer mvar
  modifyReference_ mvar $ return . (bufferLineEditor . parentTextEditor .~ this)
  return (oldst, this)

-- not for export: returns an internal-use-only data type, also discards an exception from
-- runGapBufferNew.
textBufferStateFromVector
  :: (ModifyReference mvar m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => tags -- ^ Specify the default @tags@ value.
  -> LineEditor (mvar (PrimState m)) tags
  -> GapBuffer (MVec.MVector (PrimState m) (TextLine tags)) m ()
  -> MVec.MVector (PrimState m) (TextLine tags)
  -> m (TextBufferState (mvar (PrimState m)) tags)
textBufferStateFromVector tags cur initBuf newBuf = do
  gbst <- snd <$> runGapBufferNew newBuf initBuf -- discard any exceptions that may have occurred
  return TextBufferState
    { theBufferDefaultLine   = TextLine
        { theTextLineTags        = tags
        , theTextLineString      = mempty
        , theTextLineBreakSymbol = NoLineBreak
        }
    , theBufferLineBreaker   = lineBreakerNLCR
    , theTextGapBufferState  = gbst
    , theBufferLineEditor    = cur
    , theBufferTargetCol     = 1
    }

-- Use this to initialize a new empty 'LineEditor'. This is usually only handy if you want to
-- define and test your own 'EditLine' functions and need to evaluate 'editLine' by hand rather than
-- allowing the 'TextEdit' APIs automatically manage line editing. A 'TextBufferState' always
-- contains one empty line, but a line must have a @tags@ tag, so it is necessary to pass an
-- initializing tag value of type @tags@.
--
-- See also the 'newLineEditor' function which calls this function within a 'MonadEditLine' context.
newLineEditor
  :: ModifyReference mvar m
  => Maybe VecLength -> tags -> m (LineEditor (mvar (PrimState m)) tags)
newLineEditor initSize tag =
  UMVec.new (maybe defaultInitLineBufferSize fromLength initSize) >>=
  lineEditorFromVector tag (pure ())

lineEditorFromVector
  :: ModifyReference mvar m
  => tags
  -> GapBuffer (UMVec.MVector (PrimState m) Char) m ()
  -> UMVec.MVector (PrimState m) Char
  -> m (LineEditor (mvar (PrimState m)) tags)
lineEditorFromVector tag initBuf newBuf = do
  gbst <- snd <$> runGapBufferNew newBuf initBuf
  return LineEditor
    { theParentTextEditor  = error "internal: 'LineEditor{theParentTextEditor}' is undefined"
    , theLineEditGapBuffer = gbst
    , theLineEditorIsClean = False
    , theCursorBreakSymbol = NoLineBreak
    , theLineEditorTags    = tag
    }  

-- Use this to create a deep-copy of a 'LineEditor'. The cursor location within the 'LineEditor'
-- is also copied.
copyLineEditor
  :: (ModifyReference mvar m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => LineEditor (mvar (PrimState m)) tags -> m (LineEditor (mvar (PrimState m)) tags)
copyLineEditor cur = do
  gbst <- snd <$> runGapBuffer (cloneGapBufferState >>= put) (cur ^. lineEditGapBuffer)
  return cur{ theLineEditGapBuffer = gbst }

-- | A pure function you can use to determine how many characters have been stored into this
-- buffer. This function returns an 'Int' value because it is generally not to be used with other
-- functions in this module. The 'lineEditorLength' function returns a @'Relative' 'CharIndex'@ that
-- can be used with other functions.
lineEditorCharCount :: LineEditor mvar tags -> Int
lineEditorCharCount line = fromLength count + adjust where
  buf    = line ^. lineEditGapBuffer
  count  = buf ^. gapBufferBeforeCursor + buf ^. gapBufferAfterCursor
  adjust = case line ^. cursorBreakSymbol of
    NoLineBreak -> 0
    sym         -> lineBreakSize sym - 1

-- | This funcion returns the same value as 'textLineCursorSpan' but for a 'LineEditor'.
lineEditorUnitCount :: LineEditor mvar tags -> Relative CharIndex
lineEditorUnitCount = relative . gapBufferLength . theLineEditGapBuffer

-- | Gets the 'lineEditorCharCount' value for the current 'LineEditor'.
getLineCharCount :: PrimMonad m => EditText mvar tags m Int
getLineCharCount = lineEditorCharCount <$> editTextState (use bufferLineEditor)

-- | Gets the 'lineEditorUnitCount' value for the current 'LineEditor'.
getUnitCount :: PrimMonad m => EditText mvar tags m (Relative CharIndex)
getUnitCount = lineEditorUnitCount <$> editTextState (use bufferLineEditor)

-- TODO: uncomment this, rewrite it
--
---- | Create a new 'LineEditor' by copying the line under the given 'TextLocation' point. The
---- 'LineEditor' can be updated with an 'EditLine' function. Note that this function works in any
---- monadic function type @m@ which instantiates 'Control.Monad.IO.Class.MonadIO, so this will work
---- in the @IO@ monad, the 'EditText' monad, the 'EditLine' monad, and in other contexts as well.
--newLineEditorAt
--  :: PrimMonad m
--  => TextBuffer mvar tags -> TextLocation -> m (Either TextEditError (LineEditor mvar tags))
--newLineEditorAt parent loc = liftIO $ flip runEditTextIO parent $ do
--  -- This function is safe to export because we assume a 'TextLine' never contains more than one
--  -- line terminator, and always only at the end of the buffer. The 'TextLine' constructor is not
--  -- exported.
--  i <- absoluteIndex (loc ^. lineIndex) >>= validateIndex
--  line <- getElemIndex i
--  liftIO $ do
--    let myname = "newLineEditorAt"
--    let str = ("textLineString", line ^. textLineString)
--    let strlen = intSize $ snd str
--    let breakSize = line ^. textLineBreakSymbol
--    let cur = charToIndex $ loc ^. charIndex
--    cur <- pure $ min (max 0 $ strlen - fromIntegral breakSize) $ max 0 cur
--    let buflen = head $ takeWhile (< strlen) $ iterate (* 2) 1024
--    newbuf <- liftIO $ (,) "newbuf" <$> UMVec.new buflen
--    UVec.copy (asrtMSlice SafeOp myname asrtZero (asrtShow "cur" cur) newbuf)
--              (asrtSlice  SafeOp myname asrtZero (asrtShow "cur" cur) str)
--    UVec.copy (asrtMSlice SafeOp myname (asrtShow "buflen-cur+strlen" $ buflen - cur + strlen) (asrtShow "cur-strlen" $ cur - strlen) newbuf)
--              (asrtSlice  SafeOp myname (asrtShow "cur" cur) (asrtShow "strlen-cur" $ strlen - cur) str)
--    return LineEditor
--      { theLineEditBuffer      = snd newbuf
--      , theParentTextEditor    = parent
--      , theCharsBeforeCursor   = cur
--      , theCharsAfterCursor    = strlen - cur - fromIntegral breakSize
--      , theLineEditorIsClean   = False
--      , theCharCursorIsDefined = False
--      , theCursorBreakSize     = breakSize
--      , theLineEditorTags      = theTextLineTags line
--      }

----------------------------------------------------------------------------------------------------

type family EditorTagsType (editor :: * -> *)
type instance EditorTagsType (EditText mvar tags m) = tags

class Monad editor => CursorIndexedText editor where
  getLineNumber :: editor (Absolute LineIndex)
  getLineIndex  :: EditorTagsType editor ~ tags => Absolute LineIndex -> editor (TextLine tags)

instance
  (ModifyReference mvar m, primst ~ PrimState m, VarPrimState (mvar (PrimState m)) ~ primst
  ) => CursorIndexedText (EditText (mvar primst) tags m)
  where
    getLineIndex  = editTextGetLineIndex
    getLineNumber = editTextGetLineNumber

-- | This class provides functions that can be used by both 'EditLine' and 'ViewLine'.
class Monad editor => CursorIndexedLine editor where
  getLineBreakSize  :: editor VecLength
  getLineAllocation :: editor VecLength

instance
  (ModifyReference mvar m, primst ~ PrimState m, VarPrimState (mvar (PrimState m)) ~ primst
  ) => CursorIndexedLine (EditLine (mvar primst) tags m)
  where
    getLineBreakSize  = lineBreakSize <$> editLineState (use cursorBreakSymbol)
    getLineAllocation = editLineLiftGapBuffer getAllocSize

instance Monad m => CursorIndexedLine (ViewLine tags m) where
  getLineBreakSize  = lineBreakSize . theTextLineBreakSymbol <$> use viewerLine
  getLineAllocation = viewLineLiftIIBuffer getAllocSize

-- Not for export
--
-- Returns 'True' if the given @'Absolute' 'CharIndex'@ value refers to any character after the
-- final non-line-breaking character in the current line, i.e. it points to any of the line-breaking
-- characters. If there are no line breaking characters on the current line, this function always
-- returns 'False'.
pointContainsLineBreak :: CursorIndexedLine editor => VecIndex -> editor Bool
pointContainsLineBreak pt = do
  lbrk <- getLineBreakSize
  if lbrk == 0 then return False else do
    size <- getLineAllocation
    return $ lbrk /= 0 && pt > indexAfterRange 0 (size - lbrk)

----------------------------------------------------------------------------------------------------

-- | Push a 'TextLine' before or after the cursor. This function does not effect the content of the
-- 'bufferLineEditor'.
pushLine
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => RelativeToCursor -> TextLine tags -> EditText (mvar (PrimState m)) tags m ()
pushLine rel = editTextLiftGapBuffer . pushElem rel

-- | Pop a 'TextLine' from before or after the cursor. This function does not effect the content of
-- the 'bufferLineEditor'. If you 'popLine' from 'Before' the cursor when the 'bufferLineEditor' is
-- at the beginning of the buffer, or if you 'popLine' from 'After' the cursor when the
-- 'bufferLineEditor' is at the end of the buffer, this function evaluates to an 'EndOfLineBuffer'
-- exception.
popLine
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => RelativeToCursor -> EditText (mvar (PrimState m)) tags m (TextLine tags)
popLine = editTextLiftGapBuffer . popElem

----------------------------------------------------------------------------------------------------

-- Not for export: I don't want to encourage the use of a self-reference within an 'EditText'
-- function. It is already easy enough to cause a deadlock by evaluating 'runEditText' with 'liftIO'
-- within a 'runEditText' function.
--
-- Obtain a reference to the 'TextBuffer' that this was given to this function's monadic evaluator.
thisTextBuffer :: Monad m => EditText mvar tags m (TextBuffer mvar tags)
thisTextBuffer = EditText ask

-- | Get the current line number of the cursor.
editTextGetLineNumber
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => EditText (mvar (PrimState m)) tags m (Absolute LineIndex)
editTextGetLineNumber = editTextLiftGapBuffer $ cursorIndex >>= indexToAbsolute

-- | Get the current column number of the cursor.
getColumnNumber
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => EditLine (mvar (PrimState m)) tags m (Absolute CharIndex)
getColumnNumber = editLineLiftGapBuffer $ cursorIndex >>= indexToAbsolute

-- | Get the current cursor location. This function is identical to 'getLocation'.
getLocation
  :: ModifyReference mvar m
  => EditText (mvar (PrimState m)) tags m TextLocation
getLocation = TextLocation <$> getLineNumber <*> editLine getColumnNumber

-- | Create a copy of the 'bufferLineEditor'.
copyLineEditorText
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => EditLine (mvar (PrimState m)) tags m (TextLine tags)
copyLineEditorText = TextLine
  <$> editLineLiftGapBuffer freezeVector
  <*> editLineState (use cursorBreakSymbol)
  <*> editLineState (use lineEditorTags)

-- TODO: make this more abstract, make it callable from within any 'CursorIndexedLine' monad.
validateRelativeChar
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => Relative CharIndex -> EditLine (mvar (PrimState m)) tags m VecIndex
validateRelativeChar rel =
  flip shiftAbsolute rel <$> getColumnNumber >>= editLineLiftGapBuffer . validateIndex

-- not for export
makeLineWithSlice
  :: PrimMonad m
  => (LineBreakSymbol -> LineBreakSymbol) -> CharVector -> EditLine mvar tags m (TextLine tags)
makeLineWithSlice onLbrk slice = do
  lbrk  <- editLineState $ use cursorBreakSymbol
  tags  <- editLineState $ use lineEditorTags
  return $ textLineBreakSymbol %~ onLbrk $ TextLine
    { theTextLineString      = slice
    , theTextLineTags        = tags
    , theTextLineBreakSymbol = lbrk
    }

-- | Create a 'TextLine' by copying the characters relative to the cursor.
copyChars
  :: ModifyReference mvar m
  => Relative CharIndex -> EditLine (mvar (PrimState m)) tags m (TextLine tags)
copyChars rel = do
  break <- validateRelativeChar rel >>= pointContainsLineBreak
  editLineLiftGapBuffer (sliceFromCursor (unwrapRelative rel) >>= safeFreeze) >>=
    makeLineWithSlice (if break then id else const NoLineBreak)

-- | Calls 'copyChars' with a @'Relative' 'CharIndex'@ value equal to the number of characters
-- 'Before' or 'After' the cursor on the current line.
copyCharsToEnd
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => RelativeToCursor -> EditLine (mvar (PrimState m)) tags m (TextLine tags)
copyCharsToEnd rel = editLineLiftGapBuffer (getSlice rel >>= safeFreeze) >>= case rel of
  Before -> makeLineWithSlice (const NoLineBreak)
  After  -> makeLineWithSlice id

-- | Like 'textLineIndex', but throws a 'LineIndexOutOfRange' exception if the given 'LineIndex' is
-- out of bounds.
validateCharIndex
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => Absolute CharIndex -> EditLine (mvar (PrimState m)) tags m (Absolute CharIndex)
validateCharIndex = editLineLiftGapBuffer . (validateIndex >=> indexToAbsolute)

-- | Create a 'TextLine' by copying the the characters in the given range from the line under the
-- cursor.
copyCharsRange
  :: ModifyReference mvar m
  => Absolute CharIndex -> Absolute CharIndex
  -> EditLine (mvar (PrimState m)) tags m (TextLine tags)
copyCharsRange from to = do
  (from, to) <- editLineLiftGapBuffer $ (,) <$> validateIndex from <*> validateIndex to
  break <- pointContainsLineBreak to
  slice <- editLineLiftGapBuffer $
    (,) <$> absoluteIndex from <*> absoluteIndex to >>=uncurry copyRegion >>= lift . UVec.freeze
  makeLineWithSlice (if break then id else const NoLineBreak) slice

-- | Create a 'TextLine' by copying the the characters in between the two given indicies from the
-- line under the cursor. The characters on the two given indicies are included in the resulting
-- 'TextLine'.
copyCharsBetween
  :: ModifyReference mvar m
  => Absolute CharIndex -> Absolute CharIndex
  -> EditLine (mvar (PrimState m)) tags m (TextLine tags)
copyCharsBetween from to = copyCharsRange (min from to) (max from to)

-- | Read a 'TextLine' from an @('Absolute' 'LineIndex')@ address.
editTextGetLineIndex
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => Absolute LineIndex
  -> EditText (mvar (PrimState m)) tags m (TextLine tags)
editTextGetLineIndex = editTextLiftGapBuffer . (validateIndex >=> absoluteIndex >=> getElemIndex)

-- | Write a 'TextLine' (as produced by 'copyLineEditorText' or getLineIndex') to an @('Absolute'
-- 'LineIndex')@ address.
putLineIndex
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => Absolute LineIndex -> TextLine tags -> EditText (mvar (PrimState m)) tags m ()
putLineIndex i line = editTextLiftGapBuffer $
  validateIndex i >>= absoluteIndex >>= flip putElemIndex line

-- | Replace the content in the 'bufferLineEditor' with the content in the given 'TextLine'. Pass
-- an integer value indicating where the cursor location should be set. This function does not
-- re-allocate the current line editor buffer unless it is too small to hold all of the characters
-- in the given 'TextLine', meaning this function only grows the buffer memory allocation, it never
-- shrinks the memory allocation.
refillLineEditor
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => EditText (mvar (PrimState m)) tags m ()
refillLineEditor = editTextLiftGapBuffer (getElem Before) >>= refillLineEditorWith

-- | Like 'refillLineEditor', but replaces the content in the 'bufferLineEditor' with the content in
-- a given 'TextLine', rather the content of the current line.
refillLineEditorWith
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => TextLine tags -> EditText (mvar (PrimState m)) tags m ()
refillLineEditorWith = \ case
  TextLineUndefined -> error
    "internal error: evaluated (refillLineEditorWith TextLineUndefined)"
  line              -> (>>= (throwError ||| return)) $ editLine $ do
    cur <- getColumnNumber
    flip runViewLineT line $ viewLineLiftIIBuffer $ do
      testIndex cur
      withFullSlices $ \ srcLo srcHi -> lift $ editLineLiftGapBuffer $ do
        gapBufferBeforeCursor .= sliceSize UVec.length srcLo
        gapBufferAfterCursor  .= sliceSize UVec.length srcHi
        withFullSlices $ \ targLo targHi -> lift $
          safeCopy targLo srcLo >> safeCopy targHi srcHi

-- | Delete the content of the 'bufferLineEditor' except for the line breaking characters (if any)
-- at the end of the line. This function does not change the memory allocation for the 'LineEditor',
-- it simply sets the character count to zero. Tags on this line are not effected.
clearLineEditor
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => EditLine (mvar (PrimState m)) tags m ()
clearLineEditor = do
  sym <- editLineState $ use cursorBreakSymbol
  editLineLiftGapBuffer $ do
    modCount Before $ const 0
    modCount After  $ const $ case sym of { NoLineBreak -> 0; _ -> 1; }
    return ()

liftEditText :: Monad m => EditText mvar tags m a -> EditLine mvar tags m a
liftEditText = EditLine . lift . lift

-- | Like 'clearLineEditor', this function deletes the content of the 'bufferLineEditor', and tags
-- on this line are not effected. The difference is that this function replaces the
-- 'bufferLineEditor' with a new empty line, resetting the line editor buffer to the default
-- allocation size and allowing the garbage collector to delete the previous allocation. This means
-- the line editor buffer memory allocation may be shrunk to it's minimal/default size.
resetLineEditor
  :: ModifyReference mvar m
  => EditText (mvar (PrimState m)) tags m ()
resetLineEditor = editTextState (use $ bufferLineEditor . lineEditorTags) >>=
  lift . newLineEditor Nothing >>= editTextState . assign bufferLineEditor

-- | This function copies the current 'LineEditor' state back to the 'TextBuffer', and sets a flag
-- indicating that the content of the 'LineEditor' and the content of the current line are identical
-- so as to prevent further copying. Note that this function is called automatically by
-- 'flushRefill'. Ordinarily, you would call 'flushRefill', especially when calling the 'moveByLine'
-- or 'gotoLine' functions, rather of using this function directly.
--
-- In situations where you would /not/ evaluate 'moveByLine' or 'gotoLine' with the 'flushRefill'
-- function, you can choose when to copy the content of the 'LineEditor' back to the buffer after
-- moving it. This can be done to "move" the content of a line out of the 'TextBuffer', into the
-- 'LineEditor', and then move the content back into the 'TextBuffer' at a different location after
-- changing location. This can also be useful after accumulating the content of several lines of
-- text into the 'LineEditor'.
flushLineEditor
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => EditText (mvar (PrimState m)) tags m (TextLine tags)
flushLineEditor = do
  clean <- editTextState $ use $ bufferLineEditor . lineEditorIsClean
  if clean then editTextLiftGapBuffer $ getElem Before else do
    line <- editLine copyLineEditorText
    editTextLiftGapBuffer $ putElem Before line
    editTextState $ bufferLineEditor . lineEditorIsClean .= True
    return line

-- | Evaluate a function on the 'TextLine' currently under the 'getLineNumber'. This function
-- throws an exception if the 'TextBuffer' is empty. __NOTE__ that the 'TextBuffer' is considered
-- empty if there are characters in the 'LineBuffer' which have not been flushed to the empty
-- 'TextBuffer' by either 'flushLineEditor' or 'flushRefill'.
withCurrentLine
  :: ModifyReference mvar m
  => (Absolute LineIndex -> TextLine tags -> EditLine (mvar (PrimState m)) tags m a)
  -> EditText (mvar (PrimState m)) tags m a
withCurrentLine f = do
  (ln, line) <- (,) <$> getLineNumber <*> editTextLiftGapBuffer (getElem Before)
  case line of
    TextLineUndefined -> error $
      "internal error: "++show ln++" received from 'getLineIndex' points to undefined line"
    line              -> editLine $ f ln line

-- | Return the number of lines of text in this buffer.
bufferLineCount
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => EditText (mvar (PrimState m)) tags m VecLength
bufferLineCount = editTextLiftGapBuffer countDefined

-- | Returns a boolean indicating whether there is no content in the current buffer.
bufferIsEmpty
  :: ModifyReference mvar m
  => EditText (mvar (PrimState m)) tags m Bool
bufferIsEmpty = bufferLineCount >>= \ ln ->
  if ln > 1 then return False
  else if ln <= 0 then return True
  else withCurrentLine $ \ _ line -> return $ textLineCursorSpan line == 0

-- | Return a pointer to the buffer currently being edited by the @editor@ function.
currentBuffer
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => EditText (mvar (PrimState m)) tags m (TextBuffer (mvar (PrimState m)) tags)
currentBuffer = editTextState $ use $ bufferLineEditor . parentTextEditor

----------------------------------------------------------------------------------------------------

---- not for export
--modifyColumn
--  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
--  => (Absolute CharIndex -> Absolute CharIndex -> Relative CharIndex)
--  -> EditLine (mvar (PrimState m)) tags m (Absolute CharIndex)
--modifyColumn f = do
--  lbrksym <- editLineState $ use cursorBreakSymbol
--  editLineLiftGapBuffer $ do
--    oldch  <- cursorIndex >>= indexToAbsolute
--    weight <- countDefined
--    top    <- indexToAbsolute $ indexAfterRange 0 $ weight -
--      case lbrksym of { NoLineBreak -> 0; sym -> 1; }
--    shiftCursor $ unwrapRelative $ f oldch top
--    indexNearCursor Before >>= indexToAbsolute

-- | Usually when you move the cursor to a different line using 'gotoLocation', 'gotoLine',
-- 'moveByLine', or 'moveCursor', you expect the content of the 'LineEditor' to remain on the
-- current line and you expect the 'LineEditor' will begin editing the line at the new location to
-- where it moved -- but unless you evaluate your cursor motion with the 'flushRefill' function, this
-- is not the default cursor motion behavior. The 'LineEditor' must be explicitly instructed to
-- flush it's contents to the current line using 'flushLineEditor', and load the content of the new
-- line after moving using 'refillLineEditor'.
--
-- That is precisely what this function call does: it evaluates an @editor@ function (one that
-- usually evaluates a cursor motion), but before it does, it evaluate 'flushLineEditor', and after
-- evaluating 'flushRefill' it evaluates 'refillLineEditor'.
flushRefill
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => EditText (mvar (PrimState m)) tags m a
  -> EditText (mvar (PrimState m)) tags m a
flushRefill motion = flushLineEditor >> motion <* refillLineEditor

-- | This function calls 'moveByLine' and then 'moveByChar' to move the cursor by a number of lines
-- and characters relative to the current cursor location.
--
-- __WARNING__: This evaluates 'flushLineEditor' before moving the cursor, and evaluates
-- 'refillLineEditor' after moving the cursor. This is necessary because it is impossible to
-- instruct the 'LineEditor' which character location to move to unless it contains an accurate copy
-- of the line of the buffer that it is currently editing.
--
-- If you want to move the cursor without flushing and refilling, use 'gotoLine' or
-- 'moveByLine'. Then once you have decided what to do with the content of the current line editor,
-- can move the cursor column with 'gotoChar' or 'moveByChar', or set the new target column with the
-- expression @('bufferTargetCol' 'Control.Lens..=' c)@.
moveCursor
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => Relative LineIndex -> Relative CharIndex
  -> EditText (mvar (PrimState m)) tags m TextLocation
moveCursor row col = TextLocation <$> flushRefill (moveByLine row) <*> moveByChar col

-- | Move the cursor to a different line by an @n :: Int@ number of lines. A negative @n@ indicates
-- moving the cursor toward the start of the buffer, a positive @n@ indicates moving the cursor
-- toward the end of the buffer.
--
-- This function __DOES NOT__ evaluate 'flushLineEditor' or 'refillLineEditor'.
moveByLine
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => Relative LineIndex
  -> EditText (mvar (PrimState m)) tags m (Absolute LineIndex)
moveByLine = editTextLiftGapBuffer . moveCursorBy

-- | Move the cursor to a different character location within the 'bufferLineEditor' by an @n ::
-- Int@ number of characters. A negative @n@ indicates moving toward the start of the line, a
-- positive @n@ indicates moving toward the end of the line.
--
-- This function does not wrap the cursor if motion moves past the end or beginning of the line, to
-- do this, evaluate 'moveByCharWrap'.
--
-- This function __DOES NOT__ evaluate 'flushLineEditor' or 'refillLineEditor'.
moveByChar
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => Relative CharIndex
  -> EditText (mvar (PrimState m)) tags m (Absolute CharIndex)
moveByChar = editLine . editLineLiftGapBuffer . moveCursorBy

-- | Go to an absolute line number, the first line is 1 (line 0 and below all send the cursor to
-- line 1), the last line is 'Prelude.maxBound'. This function throws an exception if the given
-- @'Absolute' 'LineIndex'@ is out of bounds. The 'goNearLine' function will move to the nearest
-- in-bounds line without ever throwing an exception.
--
-- This function __DOES NOT__ evaluate 'flushLineEditor' or 'refillLineEditor', so if you evaluate
-- this function before calling 'flushLineEditor', the changes made to the current line will not
-- remain in place on the current line, rather they will be carried over to the new line to which
-- 'gotoLine' has moved the cursor.
gotoLine
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => Absolute LineIndex
  -> EditText (mvar (PrimState m)) tags m (Absolute LineIndex)
gotoLine = editTextLiftGapBuffer . moveCursorTo

-- | Go to an absolute character (column) number, the first character is 1 (character 0 and below
-- all send the cursor to column 1), the last line is 'Prelude.maxBound'. This function throws an
-- exception if the given @'Absolute' 'CharIndex'@ is out of bounds. The 'goNearChar' function will
-- move to the nearest in-bounds line without ever throwing an exception.
--
-- This function __DOES NOT__ evaluate 'flushLineEditor' or 'refillLineEditor'.
gotoChar
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => Absolute CharIndex
  -> EditText (mvar (PrimState m)) tags m (Absolute CharIndex)
gotoChar = editLine . editLineLiftGapBuffer . moveCursorTo

-- | Go to an absolute line number, the first line is 1 (line 0 and below all send the cursor to
-- line 1), the last line is 'Prelude.maxBound'. Unlike 'gotoLine
--
-- This function __DOES NOT__ evaluate 'flushLineEditor' or 'refillLineEditor', so if you evaluate
-- this function before calling 'flushLineEditor', the changes made to the current line will not
-- remain in place on the current line, rather they will be carried over to the new line to which
-- 'gotoLine' has moved the cursor.
goNearLine
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => Absolute LineIndex -> EditText (mvar (PrimState m)) tags m (Absolute LineIndex)
goNearLine = editTextLiftGapBuffer . moveCursorNear

-- | Go to an absolute character (column) number, the first character is 1 (character 0 and below
-- all send the cursor to column 1), the last line is 'Prelude.maxBound'.
--
-- This function __DOES NOT__ evaluate 'flushLineEditor' or 'refillLineEditor'.
goNearChar
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => Absolute CharIndex
  -> EditText (mvar (PrimState m)) tags m (Absolute CharIndex)
goNearChar = editLine . editLineLiftGapBuffer . moveCursorNear

----------------------------------------------------------------------------------------------------

-- | Insert a single character. If the 'lineBreakPredicate' function evaluates to 'Prelude.True',
-- meaning the given character is a line breaking character, this function does nothing. To insert
-- line breaks, using 'insertString'. Returns the number of characters added to the buffer.
insertChar
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => RelativeToCursor -> Char
  -> EditText (mvar (PrimState m)) tags m TextCursorSpan
insertChar rel c = TextCursorSpan <$> do
  isBreak <- editTextState $ use $ bufferLineBreaker . lineBreakPredicate
  if isBreak c then return 0 else do
    editLine $ editLineLiftGapBuffer $ pushElem rel c
    editTextState $ bufferLineEditor . lineEditorIsClean .= False
    return 1

-- | This function only deletes characters on the current line, if the cursor is at the start of the
-- line and you evaluate @'deleteChars' 'Before'@, this function does nothing. The sign of the
-- 'CharIndex' given will determine the direction of travel for the deletion -- negative will delete
-- moving toward the beginning of the line, positive will delete moving toward the end of the
-- line. This function never deletes line breaking characters, even if you delete toward the end of
-- the line. This function returns the number of characters actually deleted as a negative number
-- (or zero), indicating a change in the number of characters in the buffer.
deleteChars
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => TextCursorSpan -> EditLine (mvar (PrimState m)) tags m CharStats
deleteChars req@(TextCursorSpan curspan0) = do
  let curspan = VecLength curspan0
  let done count = return (count, editLineState $ when (count /= 0) $ lineEditorIsClean .= False)
  sym <- editLineState $ use cursorBreakSymbol
  (VecLength count, setUncleanFlag) <- editLineLiftGapBuffer $
    if      req < 0 then do
      count <- validateLength curspan
      modCount Before (+ count) >> done count
    else if req > 0 then do
      let adjust n = when (sym == NoLineBreak) $ void $ modCount After (+ n)
      adjust (-1)
      count <- validateLength curspan
      modCount After $ subtract count
      adjust 1
      done count
    else return (0, pure ())
  setUncleanFlag
  return CharStats
    { cursorStepCount = signum req * TextCursorSpan count
    , deltaCharCount  = Relative $ CharIndex $ negate count
    }

-- | This function evaluates the 'lineBreaker' function on the given string, and beginning from the
-- current cursor location, begins inserting all the lines of text produced by the 'lineBreaker'
-- function.
insertString
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => String -> EditText (mvar (PrimState m)) tags m CharStats
insertString str = do
  breaker <- editTextState $ use $ bufferLineBreaker . lineBreaker
  let writeStr = editLine . editLineLiftGapBuffer .
        fmap sum . mapM ((>> (return 1)) . pushElem Before)
  let writeLine (str, lbrk) = do
        strlen <- writeStr (str ++ "\n")
        line   <- editLine $ do
          editLineState $ cursorBreakSymbol .= lbrk
          copyLineEditorText <* editLineState (charsBeforeCursor .= 0 >> charsAfterCursor .= 0)
        when (lbrk /= NoLineBreak) $ editTextLiftGapBuffer $ pushElem Before line
        editTextState $ bufferLineEditor . lineEditorIsClean .= (lbrk /= NoLineBreak)
        return CharStats
          { cursorStepCount = strlen + if lbrk == NoLineBreak then 0 else 1
          , deltaCharCount  = toLength (unwrapTextCursorSpan strlen) + lineBreakSize lbrk
          }
  let loop count = seq count . \ case
        []        -> return count
        line:more -> writeLine line >>= flip loop more . mappend count
  loop mempty $ breaker str

-- | Same as 'lineBreakWith', but uses the 'defaultLineBreak' value to break the line at the current
-- cursor location.
lineBreak
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => RelativeToCursor -> EditText (mvar (PrimState m)) tags m ()
lineBreak rel =
  editTextState (use $ bufferLineBreaker . defaultLineBreak) >>= flip lineBreakWith rel

-- | Breaks the current line editor either 'Before' or 'After' the cursor using the given
-- 'LineBreakSymbol', creating a new 'TextLine' and pushing it to the 'TextBuffer' either 'Before'
-- or 'After' the cursor.
lineBreakWith
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => LineBreakSymbol -> RelativeToCursor
  -> EditText (mvar (PrimState m)) tags m ()
lineBreakWith newlbrk rel = join $ editLine $ do
  tags    <- editLineState $ use lineEditorTags
  oldlbrk <- editLineState $ use cursorBreakSymbol
  editLineLiftGapBuffer $ case rel of
    Before -> do
      pushElem Before '\n'
      withFullSlices $ \ lo _hi -> do
        lo <- safeFreeze lo
        modCount Before $ const 0
        return $ pushLine Before $ TextLine
          { theTextLineTags        = tags
          , theTextLineString      = lo
          , theTextLineBreakSymbol = newlbrk
          }
    After  -> do
      withFullSlices $ \ _lo hi -> do
        hi <- safeFreeze hi
        modCount After $ const $ if oldlbrk == NoLineBreak then 0 else 1
        return $ pushLine Before $ TextLine
          { theTextLineTags        = tags
          , theTextLineString      = hi
          , theTextLineBreakSymbol = newlbrk
          }

----------------------------------------------------------------------------------------------------

-- | This value denotes the location of some text, and can also be used to position the cursor when
-- used with 'gotoLocation'.
data TextLocation
  = TextLocation
    { theLocationLineIndex :: !(Absolute LineIndex)
    , theLocationCharIndex :: !(Absolute CharIndex)
    }
  deriving (Eq, Ord)

instance Bounded TextLocation where
  minBound = TextLocation{ theLocationLineIndex = minBound, theLocationCharIndex = minBound }
  maxBound = TextLocation{ theLocationLineIndex = maxBound, theLocationCharIndex = maxBound }

instance Show TextLocation where
  show (TextLocation
        { theLocationLineIndex=(Absolute (LineIndex line))
        , theLocationCharIndex=(Absolute (CharIndex char))
        }) = let sh i =
                   if i == minBound then "minBound"
                   else if i == maxBound then "maxBound"
                   else show i
             in  "TextLocation "++sh line++' ':sh char

-- | Compute the difference between two 'TextLocations', works just like vector arithmetic.
diffTextLocation :: TextLocation -> TextLocation -> TextLocation
diffTextLocation a b = TextLocation
  { theLocationLineIndex = theLocationLineIndex a - theLocationLineIndex b
  , theLocationCharIndex = theLocationCharIndex a - theLocationCharIndex b
  }

sumTextLocation :: TextLocation -> TextLocation -> TextLocation
sumTextLocation a b = TextLocation
  { theLocationLineIndex = theLocationLineIndex a + theLocationLineIndex b
  , theLocationCharIndex = theLocationCharIndex a + theLocationCharIndex b
  }

relativeLine :: RelativeToCursor -> Int -> Relative LineIndex
relativeChar :: RelativeToCursor -> Int -> Relative CharIndex
(relativeLine, relativeChar) = (f LineIndex, f CharIndex) where
  f constr = ((Relative . constr) .) . \ case { Before -> negate; After -> id; }

lineIndex :: Lens' TextLocation (Absolute LineIndex)
lineIndex = lens theLocationLineIndex $ \ a b -> a{ theLocationLineIndex = b }

charIndex :: Lens' TextLocation (Absolute CharIndex)
charIndex = lens theLocationCharIndex $ \ a b -> a{ theLocationCharIndex = b }

-- | This function calls 'gotoLine' and then 'gotoChar' to move the cursor to an absolute a line
-- number and characters (column) number.
--
-- __WARNING__: This evaluates 'flushLineEditor' before moving the cursor, and evaluates
-- 'refillLineEditor' after moving the cursor. This is necessary because it is impossible to
-- instruct the 'LineEditor' which character location to move to unless it contains an accurate copy
-- of the line of the buffer that it is currently editing.
--
-- If you want to move the cursor without flushing and refilling, use 'gotoLine' or
-- 'moveByLine'. Then once you have decided what to do with the content of the current line editor,
-- can move the cursor column with 'gotoChar' or 'moveByChar', or set the new target column with the
-- expression @('bufferTargetCol' 'Control.Lens..=' c)@.
gotoLocation
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => TextLocation -> EditText (mvar (PrimState m)) tags m TextLocation
gotoLocation (TextLocation{theLocationLineIndex=ln,theLocationCharIndex=ch}) =
  TextLocation <$> flushRefill (gotoLine ln) <*> gotoChar ch

-- | Like 'gotoLocation' but never throws an exception if the given 'TextLocation' is out of bounds.
goNearLocation
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => TextLocation -> EditText (mvar (PrimState m)) tags m TextLocation
goNearLocation (TextLocation{theLocationLineIndex=ln,theLocationCharIndex=ch}) =
  TextLocation <$> flushRefill (goNearLine ln) <*> goNearChar ch

-- | Save the location of the cursor, then evaluate an @editor@ function. After evaluation
-- completes, restore the location of the cursor (within range, as the location may no longer exist)
-- and return the result of evaluation.
saveCursorEval
  :: ModifyReference mvar m
  => EditText (mvar (PrimState m)) tags m a
  -> EditText (mvar (PrimState m)) tags m a
saveCursorEval f = do
  (cur, a) <- (,) <$> getLocation <*> f
  goNearLocation cur >> return a

class RelativeToAbsoluteCursor index editor | editor -> index where
  -- | Convert a 'Relative' index (either a 'LineIndex' or 'CharIndex') to an 'Absolute' index.
  relativeToAbsolute :: Relative index -> editor (Absolute index)

instance
  (PrimMonad m, primst ~ PrimState m, VarPrimState (mvar primst) ~ primst
  ) =>
  RelativeToAbsoluteCursor LineIndex (EditText (mvar primst) tags m)
  where
    relativeToAbsolute = editTextLiftGapBuffer . (relativeIndex >=> indexToAbsolute)

instance
  (PrimMonad m, primst ~ PrimState m, VarPrimState (mvar primst) ~ primst
  ) =>
  RelativeToAbsoluteCursor CharIndex (EditLine (mvar primst) tags m)
  where
    relativeToAbsolute = editLineLiftGapBuffer . (relativeIndex >=> indexToAbsolute)

----------------------------------------------------------------------------------------------------

-- | 'TextFrame's are a shallow, immutable snapshot of all, or a portion of, a 'TextBuffer'. The
-- internal structure of a 'TextFrame' is identical to that of the 'TextBuffer' so no additional
-- conversion or copying is necessary, making the creation of views extremely fast. Create a
-- 'TextFrame' using 'textFrameOnLines' or 'textFrameOnRange'. Once a 'TextFrame' is created other
-- threads may immediately resume performing updates on the 'TextBuffer'.
data TextFrame tags
  = TextFrame
    { theTextFrameCharCount :: !VecLength
      -- ^ Evaluates to how many characters exist in this buffer.
    , theTextFrameVector    :: !(Vec.Vector (TextLine tags))
      -- ^ Evaluates to a vector of 'TextLine's so you can make use of the 'Vec.Vector' APIs to
      -- define your own operations on a 'TextFrame'.
    }

instance Semigroup tags => Semigroup (TextFrame tags) where
  (<>) = textFrameAppend (<>)

instance Monoid tags => Monoid (TextFrame tags) where
  mempty  = TextFrame{ theTextFrameCharCount = 0, theTextFrameVector = mempty }
  mappend = textFrameAppend mappend

-- not for export
textFrameCharCount :: Lens' (TextFrame tags) VecLength
textFrameCharCount = lens theTextFrameCharCount $ \ a b -> a{ theTextFrameCharCount = b }

-- not for export
textFrameVector :: Lens' (TextFrame tags) (Vec.Vector (TextLine tags))
textFrameVector = lens theTextFrameVector $ \ a b -> a{ theTextFrameVector = b }

-- not for export: requires an uninitialized 'LineEditor' as a parameter.
--
-- This function copies the content of an immutable 'TextFrame' into a
-- mutable 'TextBuffer'.
frameToBuffer
  :: ModifyReference mvar m
  => Maybe VecLength
    -- ^ Specify the additional amount of space to allocate for the buffer. This value will be added
    -- to the number of 'TextLine' elements in the 'TextFrame' to allocate the new bufer.
  -> TextLocation -- ^ The initial cursor position.
  -> tags            -- ^ Specify the default tags value.
  -> TextFrame tags  -- ^ The frame from which to fill the buffer.
  -> LineEditor (mvar (PrimState m)) tags -- ^ An initial line editor, should be empty.
  -> m (TextBuffer (mvar (PrimState m)) tags)
frameToBuffer initSize loc tags TextFrame{ theTextFrameVector=vec } cur = do
  let i   = loc ^. lineIndex
  let len = Vec.length vec
  if len <= 0 then newTextBuffer initSize tags else do
    buf <- MVec.new (max len $ maybe defaultInitLineBufferSize fromLength initSize) >>=
      textBufferFromVector tags cur
        (do forceCursorIndex i
            withFullSlices $ \ targLo targHi -> do
              modCount Before $ const $ sliceSize MVec.length targLo
              modCount After  $ const $ sliceSize MVec.length targHi
              lift $ void $ runIIBuffer
                (do forceCursorIndex i
                    withFullSlices $ \ srcLo srcHi -> do
                      safeCopy targLo srcLo
                      safeCopy targHi srcHi
                )
                IIBufferState{ theIIBufferVector = vec, theIIBufferCursor = 0 }
        )
    flip runEditText buf $ do
      editLine $ editLineLiftGapBuffer $
        absoluteIndex (loc ^. charIndex) >>=
        modCount Before . const . distanceFromOrigin
      refillLineEditor
    return buf

-- | Create a new, editable 'TextBuffer' from a read-only 'TextFrame'. Pass 'Before' to place the
-- text in the buffer before the cursor, meaning the cursor will start locationed at the end of the
-- editable 'TextBuffer', or pass 'After' to place the text in the buffer after the cursor, meaning
-- the cursor will start at the beginning of the editable 'TextBuffer'.
newTextBufferFromFrame
  :: ModifyReference mvar m
  => TextLocation -> tags -> TextFrame tags -> m (TextBuffer (mvar (PrimState m)) tags)
newTextBufferFromFrame loc tags frame = newLineEditor Nothing tags >>=
  frameToBuffer Nothing loc tags frame

-- | Decompose a 'TextFrame' into a list of 'TextLine's.
textFrameToList :: TextFrame tags -> [TextLine tags]
textFrameToList = Vec.toList . theTextFrameVector

-- | Decompose a 'TextFrame' into a list of tagged 'String's. The 'String' is paired with a 'Word16'
-- count of the number of line-breaking characters exist at the end of the string, and the @tags@
-- assoicated with the text.
textFrameToStrings :: TextFrame tags -> [(String, tags)]
textFrameToStrings = textFrameToList >=> \ case
  TextLineUndefined -> []
  line@(TextLine{theTextLineTags=tags,theTextLineBreakSymbol=lbrksym}) ->
    [(textLineChomp line ++ show lbrksym, tags)]

-- | An empty 'TextFrame', containing zero lines of text.
emptyTextFrame :: TextFrame tags
emptyTextFrame = TextFrame{ theTextFrameCharCount = 0, theTextFrameVector = Vec.empty }

-- | This function works similar to 'Data.Monoid.mappend' or the @('Data.Semigroup.<>')@ operator,
-- but allows you to provide your own appending function for the @tags@ value. Tags are appended if
-- the final lines of the left 'TextFrame' has no line break and therefore must to be prepended to
-- the first line of the right 'TextFrame'. If the left 'TextFrame' ends with a line break, the @tags@
-- appending function is never evaluated.
textFrameAppend :: (tags -> tags -> tags) -> TextFrame tags -> TextFrame tags -> TextFrame tags
textFrameAppend appendTags
  a@(TextFrame{theTextFrameCharCount=countA,theTextFrameVector=vecA})
  b@(TextFrame{theTextFrameCharCount=countB,theTextFrameVector=vecB}) =
    let lenA = Vec.length vecA in
    let lenB = Vec.length vecB in
    if lenA == 0 then a else if lenB == 0 then b else TextFrame
    { theTextFrameCharCount = countA + countB
    , theTextFrameVector    = runST
        (do let hiA     = vecA Vec.! (lenA - 1)
            let loB     = vecB Vec.! 0
            let noBreak = hiA ^. textLineBreakSymbol == NoLineBreak
            vecAB <- MVec.new $ lenA + lenB - if noBreak then 1 else 0
            runGapBufferNew vecAB $
              if not noBreak then pushElemVec Before vecA >> pushElemVec Before vecB else do
                pushElemVec Before $ Vec.slice 0 (lenA - 1) vecA
                pushElem Before $ TextLine
                  { theTextLineString = mappend (theTextLineString hiA) (theTextLineString loB)
                  , theTextLineTags   = appendTags (theTextLineTags hiA) (theTextLineTags loB)
                  , theTextLineBreakSymbol = theTextLineBreakSymbol loB
                  }
                pushElemVec Before $ Vec.slice 1 (lenB - 1) vecB
            Vec.unsafeFreeze vecAB
        )
    }

-- | Copy a region of the current 'TextBuffer' into a 'TextFrame', delimited by the two given
-- 'TextLocation' values. If the two 'TextLocation' values given are identical, an empty 'TextFrame'
-- is constructed. The 'TextLocation' value further from the start of the 'TextBuffer' is considered
-- to be the end point of the text to be copied into the resulting 'TextFrame', and the character
-- under the end point is not included in the resulting 'TextFrame'.
textFrame
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => TextLocation -> TextLocation -> EditText (mvar (PrimState m)) tags m (TextFrame tags)
textFrame from0 to0 = do
  (from, loline) <- validateLocation $ min from0 to0
  (to,   hiline) <- validateLocation $ max from0 to0
  if (from ^. lineIndex) == (to   ^. lineIndex) then
    (throwError ||| return) $ flip runViewLine loline $ do
      viewerCursorToChar (from ^. charIndex)
      top <- viewerTopChar
      case top of
        Nothing  -> return TextFrame
          { theTextFrameCharCount = 0
          , theTextFrameVector    = Vec.empty
          }
        Just top -> do
          (lo, hi) <- splitLine
          line <- if top == (to ^. charIndex) then pure lo else
                    viewerLine .= hi >> fst <$> splitLine
          return TextFrame
            { theTextFrameCharCount = VecLength $ intSize line
            , theTextFrameVector    = Vec.singleton line
            }
   else do
     let splitter line point take = (throwError ||| return) $
           flip runViewLine line $ viewerCursorToChar (point ^. charIndex) >> take <$> splitLine
     loline <- splitter loline from snd
     hiline <- splitter hiline to   fst
     editTextLiftGapBuffer $ do
       fromIdx <- absoluteIndex $ (from ^. lineIndex) + 1
       toIdx   <- absoluteIndex $ (to   ^. lineIndex) - 1
       mvec    <- lift $ MVec.new $ fromLength $
                    (to ^. lineIndex) `diffAbsolute` (from ^. lineIndex)
       let init f = lift $ fmap fst $ runGapBufferNew mvec $
                    pushElem Before loline >> f >> pushElem Before hiline >> freezeVector
       vec <- (>>= (throwError ||| return)) $ if fromIdx > toIdx then init $ pure () else
         withRegion fromIdx toIdx $ \ lo hi -> init $ pushSlice Before lo >> pushSlice Before hi
       return TextFrame
         { theTextFrameCharCount = VecLength $ sum $ intSize <$> Vec.toList vec
         , theTextFrameVector    = vec
         }

-- | Like 'textFrame', creates a new text view, but rather than taking two 'TextLocation's to delimit
-- the range, takes two @('Absolute' 'LineIndex')@ values to delimit the range.
textFrameOnLines
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => Absolute LineIndex -> Absolute LineIndex
  -> EditText (mvar (PrimState m)) tags m (TextFrame tags)
textFrameOnLines from to = textFrame
  (TextLocation
   { theLocationLineIndex = min from to
   , theLocationCharIndex = Absolute $ CharIndex 1
   })
  (TextLocation
   { theLocationLineIndex = max from to
   , theLocationCharIndex = Absolute $ CharIndex maxBound
   })

----------------------------------------------------------------------------------------------------

-- | This is a type for functions that are used to inspect the elements of a 'TextFrame'.
newtype ViewText tags m a
  = ViewText (ExceptT TextEditError (StateT (ViewTextState tags) m) a)
  deriving (Functor, Applicative, Monad, MonadError TextEditError)

type instance EditorTagsType (ViewText tags m) = tags

instance MonadTrans (ViewText tags) where
  lift = ViewText . lift . lift

data ViewTextState tags
  = ViewTextState
    { theViewerLinesBeforeCursor :: VecLength
    , theViewerFrame             :: TextFrame tags
    }

instance PrimMonad m => TextFoldable (ViewText tags m) where
  foldLinesBetween = gFoldMapBetween
    (foldLinesLift . lift)
    viewTextLiftIIBuffer
    runFoldLinesStep
    (readSlice $ \ vec -> return . (vec Vec.!))
    (\ _ _ -> return)
    (sliceSize Vec.length)

viewerLinesBeforeCursor :: Lens' (ViewTextState tags) VecLength
viewerLinesBeforeCursor =
  lens theViewerLinesBeforeCursor $ \ a b -> a{ theViewerLinesBeforeCursor = b }

viewerFrame :: Lens' (ViewTextState tags) (TextFrame tags)
viewerFrame = lens theViewerFrame $ \ a b -> a{ theViewerFrame = b }

-- not for export
--
-- Only used by instance of foldLinesBetween
viewTextLiftIIBuffer
  :: Monad m
  => IIBuffer (Vec.Vector (TextLine tags)) m a
  -> ViewText tags m a
viewTextLiftIIBuffer f = ViewText $ ExceptT $ do
  vec <- use (viewerFrame . textFrameVector)
  cur <- use viewerLinesBeforeCursor
  (ret, st) <- lift $ flip runIIBuffer
    IIBufferState{ theIIBufferVector = vec, theIIBufferCursor = cur }
    (catchError (Right <$> f) $ fmap Left . bufferToLineEditError)
  viewerFrame . textFrameVector .= st ^. iiBufferVector
  viewerLinesBeforeCursor       .= st ^. iiBufferCursor
  case ret of
    Right ret -> return ret
    Left{}    -> error $ "internal error: 'bufferTextToEditorError' evaluated to 'throwError'"

-- | Run a function of type 'ViewText' on a 'TextFrame', allowing you to inspect the elements of the
-- frame line by line.
runViewTextT :: Monad m => ViewText tags m a -> TextFrame tags -> m (Either TextEditError a)
runViewTextT (ViewText f) frame = evalStateT (runExceptT f) ViewTextState
  { theViewerLinesBeforeCursor = 0
  , theViewerFrame             = frame
  }

-- | Like 'RunViewTextT' but evaluates to a pure function. Of course the 'ViewText' function given
-- must be pure, this the monadic type is 'Identity'.
runViewText :: ViewText tags Identity a -> TextFrame tags -> Either TextEditError a
runViewText (ViewText f) frame = evalState (runExceptT f) ViewTextState
  { theViewerLinesBeforeCursor = 0
  , theViewerFrame             = frame
  }

-- | Get the current line under the cursor within a 'ViewText' function context.
viewerGetLine :: Monad m => ViewText tags m (TextLine tags)
viewerGetLine = viewTextLiftIIBuffer $ getElem Before

-- | Move the line cursor within a 'ViewText' function context.
viewerCursorToLine :: Monad m => Absolute LineIndex -> ViewText tags m ()
viewerCursorToLine = viewTextLiftIIBuffer . void .
  (validateIndex >=> distanceFromCursor >=> modCount Before . (+))

-- | Take the line under the cursor within a 'ViewText' function context, and use this line to
-- evaluate a 'ViewLine' function.
viewLine :: Monad m => ViewLine tags m a -> ViewText tags m a
viewLine f = viewerGetLine >>= lift . runViewLineT f >>= (throwError ||| return)

----------------------------------------------------------------------------------------------------

-- | This data type will ordinarily not be useful on it's own, it should be used by way of the
-- "Hakshell.TextEditor.Parser" module, although if you need to perform special parsing operations,
-- such as running a single parser over multiple buffers, you may need to use some of the APIs here.
--
-- This data type can be thought of as a sort of "cursor" that inspects every character within a
-- 'TextBuffer', and contains the state necessary to perform parsing over a 'TextBuffer'. Streaming
-- characters from a 'TextBuffer' is very efficient, all character retreival operates in O(1) time,
-- and all characters are already in memory so there is no additional cost for look-aheads or
-- backtracking, as the cost has already been paid in advance.
--
-- Thinking of this data structure as a text cursor, the current character under the cursor value is
-- taken with the 'streamLook' function, the cursor is stepped to the next character using
-- 'streamStep', and the cursor can be moved to an arbitrary 'TextLocation' using
-- 'streamGoto'.
--
-- This data type is immutable (pure), so modifications to the state of this object must be stored
-- on the stack, or used as the mutable value of a 'State' monad, or stored in a mutable variable of
-- some form. Although you create a 'StreamCursor' cursor within a 'EditText' function evaluation
-- context, the 'StreamCursor' is not sensitive to which 'TextBuffer' is currently being inspected
-- by the 'EditText' function. This means your 'EditText' function evaluating in 'TextBuffer' @a@
-- can return the 'StreamCursor', and this same parser stream can then be used to call
-- 'streamLook' within an 'EditText' function that is currently evaluating in a different
-- 'TextBuffer' @b@. The 'StreamCursor' itself only contains the current 'TextLocation' and a cached
-- copy of the last line that it had been inspecting. If you are performing low-level programming of
-- parsers using a 'StreamCursor' it is up to you to ensure the 'StreamCursor' evaluates in the
-- expected 'TextBuffer'.
data StreamCursor tags
  = StreamCursor
    { theStreamCache    :: !(TextLine tags)
      -- ^ Get the 'TextLine' currently being cached by this 'StreamCursor' object. This cached
      -- 'TextLine' changes every time the 'streamLocation' moves to another line, or when
      -- 'streamResetCache' is evaluated within an 'EditText' function context.
    , theStreamLocation :: !TextLocation
      -- ^ Thinking of a 'StreamCursor' as a cursor pointing to a character within a 'TextBuffer'
      -- this function returns the 'TextLocation' of the cursor, similar to how 'getLocation'
      -- returns the location of the text editing cursor.
    , theStreamEndpoint :: !TextLocation
      -- ^ This is 'TextLocation' of the final character within the 'TextBuffer' from which this
      -- 'StreamCursor' is reading. If the 'TextBuffer' is modified during parsing, this value must
      -- be updated by evaluating 'streamResetEndpoint'.
    }

-- not for export
streamCache :: Lens' (StreamCursor tags) (TextLine tags)
streamCache = lens theStreamCache $ \ a b -> a{ theStreamCache = b }

-- not for export
streamLocation :: Lens' (StreamCursor tags) TextLocation
streamLocation = lens theStreamLocation $ \ a b -> a{ theStreamLocation = b }

---- not for export
--streamEndpoint :: Lens' (StreamCursor tags) TextLocation
--streamEndpoint = lens theStreamEndpoint $ \ a b -> a{ theStreamEndpoint = b }

-- | This is not a getter but a lens which you can use to update the @tags@ of the current
-- 'StreamCursor'. If you do update the @tags@, you can call 'streamCommitTags' to write these
-- tags back to the 'TextBuffer'. The 'streamCommitTags' function is called automatically by
-- the 'streamStep' function.
streamTags :: Lens' (StreamCursor tags) tags
streamTags = streamCache . textLineTags

-- | Constructs a new 'StreamCursor' at a given 'TextLocation' within a given 'TextBuffer'. This
-- function must validate the 'TextLocation' using 'testLocation', so the return type is similar in
-- meaning to the return type of 'validateLocation', which may throw a soft exception (which can be
-- caught with 'catchError') if the given 'TextLocation' is out of bounds.
newStreamCursorRange
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => TextLocation -> TextLocation -> EditText (mvar (PrimState m)) tags m (StreamCursor tags)
newStreamCursorRange start end = streamResetCache StreamCursor
  { theStreamCache    = TextLineUndefined
  , theStreamLocation = min start end
  , theStreamEndpoint = max start end
  } >>= streamResetEndpoint (max start end)

-- | A convenience function that calls 'newStreamCursorRange' with 'minBound' as the 'TextLocation'.
newStreamCursor
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => EditText (mvar (PrimState m)) tags m (StreamCursor tags)
newStreamCursor = newStreamCursorRange minBound maxBound

-- | This in an 'EditText' type of function which moves a given 'StreamCursor' to a different
-- location within the 'TextBuffer' of the current 'EditText' context. This can be used to perform
-- backtracking, or forward-tracking, or any kind of tracking.
--
-- If the given 'StreamCursor' originated in a different 'TextBuffer' (supposing the 'EditText'
-- function in which 'newStreamCursor' was evaluated returned the 'StreamCursor'), the target
-- 'TextBuffer' for the 'StreamCursor' simply changes over to the new 'TextBuffer' and begins
-- sourcing characters from there. You should probably evaluate 'streamResetCache' 
--
-- This function must validate the 'TextLocation' using 'validateLocation', and so may throw a soft
-- exception (which can be caught with 'catchError') if the given 'TextLocation' is out of bounds.
streamGoto
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => StreamCursor tags -> TextLocation
  -> EditText (mvar (PrimState m)) tags m (StreamCursor tags)
streamGoto cursor = validateLocation >=> \ (loc, txt) ->
  return cursor{ theStreamCache = txt, theStreamLocation = loc }

-- | Thinking of the 'StreamCursor' as a cursor pointing to a character within a 'TextBuffer' this
-- function returns the character currently under the cursor __without advancing the cursor.__ This
-- function throws an @'EndOfLineBuffer' 'After'@ exception if 'streamResetCache' cannot resolve
-- 'theStreamLocation'. This function throws an @'EndOfCharBuffer' 'After'@ exception if cursor has
-- somehow moved beyond 'theStreamCache', which could only happen if the 'streamResetCache' function
-- has not been called after transplanting the 'StreamCursor' to another 'TextBuffer'.
streamLook
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => StreamCursor tags -> EditText (mvar (PrimState m)) tags m (Char, StreamCursor tags)
streamLook s = case theStreamCache s of
  TextLineUndefined -> streamResetCache s >>= streamLook
  line              -> case textLineGetChar line (theStreamLocation s ^. charIndex) of
    Nothing           -> throwError $ EndOfCharBuffer After
    Just  c           -> return (c, s)
{-# INLINE streamLook #-}

-- | Thinking of the 'StreamCursor' as a cursor pointing to a character within a 'TextBuffer' this
-- function advances the cursor without reading any characters.
streamStep
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => StreamCursor tags -> EditText (mvar (PrimState m)) tags m (StreamCursor tags)
streamStep s = do
  let i   = fromIndex $ s ^. streamLocation . charIndex
  let top = intSize   $ s ^. streamCache
  when (i >= top) $ streamCommitTags s
  return $ 
    ( if i < top then streamLocation . charIndex +~ 1 else
        (streamLocation %~ (lineIndex +~ 1) . (charIndex .~ 1)) .
        (streamCache .~ TextLineUndefined)
    ) s
{-# INLINE streamStep #-}

-- | The 'streamStep' function may have placed this stream into a state indicating that the cursor
-- has stepped beyond the end of the current 'TextLine'. This function returns whether or not this
-- is the case.
streamIsEOL :: StreamCursor tags -> Bool
streamIsEOL s = case s ^. streamCache of
  TextLineUndefined -> True
  line              -> fromIndex (s ^. streamLocation . charIndex) >= intSize line
{-# INLINE streamIsEOL #-}

-- | Tests whether 'theStreamLocation' is greater-than or equal-to 'theStreamEndpoint'.
streamIsEOF :: StreamCursor tags -> Bool
streamIsEOF s = theStreamLocation s >= theStreamEndpoint s
{-# INLINE streamIsEOF #-}

-- | There are times when the 'StreamCursor' contains a cached 'TextLine' (given by the
-- 'streamCache' value) that is different from the actual 'TextLine' unders the cursor
-- location of the 'TextLocation' given by 'streamLocation'. This can happen when you evaluate
-- a 'StreamCursor' in a new 'EditText' context for a different 'TextBuffer', or if
-- 'flushLineEditor' has been evaluated and modified the line currently being inspected by the
-- 'StreamCursor'.
--
-- If you happen to know that the 'TextLine' given by 'streamCache' is different from what it
-- should be, you should evaluate this function to reset the cached 'TextLine' to the correct
-- value. This function must also check that the 'streamLocation' is still valid, so it may
-- evaluate to a @('Left' 'After')@ or @('Left' 'Before')@ value as with the 'streamGoto'
-- function.
streamResetCache
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => StreamCursor tags -> EditText (mvar (PrimState m)) tags m (StreamCursor tags)
streamResetCache s = do
  (loc, txt) <- validateLocation (theStreamLocation s)
  return (s & streamLocation .~ loc & streamCache .~ txt)

-- | This function should only need to be evaluated once as long as the 'TextBuffer' never changes
-- suring the parsing process. If the 'TextBuffer' does change while the parsing has been paused,
-- this function must be evaluated to ensure the 'StreamCursor' is aware of the endpoint.
streamResetEndpoint
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => TextLocation -> StreamCursor tags
  -> EditText (mvar (PrimState m)) tags m (StreamCursor tags)
streamResetEndpoint loc s = do
  (loc, _txt) <- validateLocation loc
  return (s & streamLocation .~ loc)

-- | You can use the 'streamTags' lens to alter the @tags@ value of the line cached (the line
-- returned by 'streamCache'd function), but to actually store these changes back to the
-- 'TextBuffer', this function must be called. This function is called automatically by the
-- 'streamStep' function when the cursor steps past the end of the current line and to the
-- next line.
streamCommitTags
  :: (PrimMonad m, VarPrimState (mvar (PrimState m)) ~ PrimState m)
  => StreamCursor tags -> EditText (mvar (PrimState m)) tags m ()
streamCommitTags s = putLineIndex (s ^. streamLocation . lineIndex) (s ^. streamCache)
