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
    --
    -- There are two general types of buffers:
    --
    --     * 'TextBuffer' which is mutable, and
    --     * 'TextFrame' which is immutable.
    --
    -- Though different, both buffer types have many things in common:
    --
    --     * store information in vector data structures from the @vector@ library,
    --     * vectors contain 'TextLine' values
    --     * provide cursors for inspecting text
    --
    -- Of course, in the case of the immutable 'TextBuffer' type, the cursors can change text
    -- content relative to the cursor position, while 'TextFrame's cannot have the text content be
    -- changed.
    --
    -- Even immutable buffers, which cannot have the text content changed, have stateful cursors
    -- that can have their positions changed. Since cursors are stateful, functions for manipulating
    -- the cursor are monadic in nature.
    --
    --     * Functions of type 'ViewText' are cursors that operate on immutable 'TextFrame's,
    --     * functions of type 'EditText' are cursors that operate on mutable 'TextBuffer's.
    --
    -- There are also functions for folding, mapping, and both folding and mapping over buffers.
    --
    --     * 'FoldLines' performs read-only folds,
    --     * 'MapLines' maps over lines in-place without changing the number of lines in the buffer,
    --     * 'FoldMapLines' can map over lines in-place without changing the number of lines in the
    --       buffer, but can also fold item into a value.

    -- ** The Cursor
    --
    -- The cursor is an index into a buffer, and index values begin at 1, not at 0, as is typical
    -- with most text editors. Most cursor functions take a 'RelativeToCursor' value which is either
    -- 'Before' or 'After'. 'Before' may have instead been called "on" or "under" because it
    -- actually indicates the element at the current cursor index, while 'After' indicates the first
    -- element after the cursror.

    Absolute(..),  LineIndex(..), CharIndex(..), TextLocation(..),
    CharCount(..), CharStats(..),
    Relative(..), RelativeToCursor(..),
    RelativeToAbsoluteCursor, -- <- does not export members
    relativeToAbsolute, relativeLine, relativeChar, lineIndex, charIndex,
    sumTextLocation, diffTextLocation,

    -- *** The 'LineEditor' data type.
    --
    -- There is a mutable 'LineEditor' data type which allows editing of individual characters in a
    -- line.  It is filled with the characters in the 'TextLine' under the cursor using
    -- 'refillLineEditor', and can be flushed back to the line under the cursor using
    -- 'flushLineEditor'. The 'TextBuffer' data type always has one 'LineEditor' built-in to it. The
    -- 'LineEditor' is flushed and filled as as the cursor moves around in the 'TextBuffer'.

    LineEditor,
    flushRefill, refillLineEditor, refillLineEditorWith, flushLineEditor,
    lineEditorCharCount, lineEditorUnitCount, getLineCharCount, getUnitCount,
    copyLineEditorText, clearWholeLine, resetLineEditor,

    -- *** Getting and setting the cursor position
    --
    -- These are the fundamental cursor motion functions. You must move by line and then move by
    -- character separately. There are more convenient cursor motion functinos below which allow
    -- motion to a line and character using a single 'TextLocation' value.
    --
    -- When moving the cursor around, some functions, like 'gotoLine' or 'moveByLine' it's contents
    -- on the current line unless you explicitly call 'flushLineEditor'. However functions like
    -- 'gotoLocation' do automatically call 'flushLineEditor' because the cursor 'CharIndex'
    -- position cannot be updated until it is known how many characters exist on the current line,
    -- and the number of characters that exist on the line cannot be known until the line is
    -- flushed.
    --
    -- Without "flushing" the characters currently in the 'LineEditor' will not be saved to the
    -- 'TextBuffer' and will be carried around to the 'LineEditor's current location, however if the
    -- cursor's column location cannot be changed unless 'refilLineEditor' is used to move the line
    -- location.
    --
    -- There are situations where you may not want to use 'flushRefill' to perform a cursor motion, as
    -- in when accumulating lines into the 'LineEditor' after each cursor motion.

    getLocation, saveCursorEval, shiftAbsolute, diffAbsolute, unwrapCharCount,
    moveByCharWrap, spanDistance, distanceBetween, 

    CursorIndexedText(..), CursorIndexedLine(..), CursorIndexedBuffer(..),

    -- ** The 'TextLine' data type
    --
    -- A 'TextBuffer' is a memory-efficient vector containing 'TextLine's. A 'TextLine' is created
    -- by evaluating a 'TextEdit' function such as 'insertString'. A 'TextLine' within a
    -- 'TextBuffer' can be updated by navigating the cursor to the line using 'gotoChar' or
    -- 'moveByChar'. Moving the cursor to an address (addresses are line numbers) will copy the
    -- 'TextLine' at that address into a 'LineEditor'. You can then edit the text in a 'LineEditor'
    -- by evaluating a function of type 'EditLine' with the 'editLine' function.

    TextLine, emptyTextLine, textLineBreakSymbol,
    nullTextLine, textLineStats, textLineGetChar,
    textLineTop, textLineIsUndefined,
    textLineTags, textLineChomp, showTextLine,

    -- *** Cursoring over characters in a 'TextLine'
    --
    -- The 'ViewLine' function type allows for manipulating a cursor along a 'TextLine' to inspect
    -- individual characters.

    ViewLine, liftViewLine, runViewLineT, runViewLine, viewerTopChar, cursorIsOnLineBreak,
    sliceLineToEnd, splitLine,

    -- ** The 'TextBuffer' data type
    --
    -- A 'TextBuffer' can be created with 'newTextBuffer', and then edited by evaluating a function
    -- of type 'EditText' on it with the 'runEditTextIO' function. You typically fill a 'TextBuffer'
    -- with a "String" using 'insertString'. A 'TextBuffer' contains a cursor which you can move
    -- around using the 'gotoChar', 'moveByChar', functions. Text is deleted with the
    -- 'deleteCharsWrap' functions.

    TextBuffer,
    newTextBuffer, copyTextBuffer,
    bufferLineBreaker, bufferDefaultTags, bufferTargetCol,
    thisTextBuffer,

    module Hakshell.TextEditor.LineBreaker,

    -- ** The 'EditText' function type
    --
    -- Note that some functions of the 'EditText' function type, like 'getLineAt',
    -- 'getLineNumber', 'getColumnNumber', are defined as members of the 'CursorIndexedText'
    -- typeclass.

    EditText, runEditText, runEditTextOnCopy,
    insertString, lineBreak, lineBreakWith, deleteCharsWrap,
    popLine, pushLine, putLineIndex, withCurrentLine,
    bufferLineCount, bufferIsEmpty, currentBuffer,
    lineCursorIsDefined, 

    -- ** The 'EditLine' function type
    --
    -- Usually, functions of type 'EditLine' are evaluated by using the 'editLine' function within
    -- the context of an 'EditText' function. This places a line of text into a 'LineEditor' which
    -- allows the text to be edited character-by-character.

    EditLine, editLine, liftEditText,
    insertTextLine, insertChar, deleteChars, lineEditorTags,
    copyCharsRange, copyCharsBetween, copyChars, copyCharsToEnd,
    charCursorIsDefined, setLineBreakSymbol,

    -- ** Indicies and Bounds Checking

    validateLineIndex, lineIndexIsValid,
    validateGetLineIndex, testLocation, validateLocation, locationIsValid,
    validateCharIndex, indexIsOnLineBreak,

    -- *** Error Data Type

    TextEditError(..),
    
    -- ** Text Frames
    --
    -- A 'TextFrame' is an immutable copy of a portion of a 'TextBuffer'. By creating a 'TextFrame'
    -- you take a snapshot of a 'TextBuffer' at a point in time which can then be pasted into
    -- another 'TextBuffer', or used to draw the window of an interactive text editor application.

    TextFrame, textFrame, textFrameOnLines, textFrameAppend, emptyTextFrame,
    newTextBufferFromFrame, textFrameCharCount, textFrameVector,
    textFrameToList, textFrameToStrings,

    -- *** 'TextFrame' Inspection Monad

    ViewText, runViewTextT, runViewText, viewLine,

    -- * Batch Editing

    -- ** A function type for rewriting multiple lines at once
    RewriteLines, writeBack, rewriteLines, rewriteLinesInRange, rewriteLinesInBuffer,

    -- ** Folding over lines of text
    --
    -- These functions perform a batch read-only opertion over the 'TextBuffer' without moving the
    -- position of the cursor. Be careful to evaluate 'flushLineEditor' before evaluating folds over
    -- 'TextBuffer's to ensure the latest changes to the 'LineEditor' are actually stored into the
    -- 'TextBuffer' and are ready to be folded, or your results may not be what you expect.

    FoldLines, foldLines, foldAllLines, foldLinesBetween, foldLinesInRange, runFoldLinesStep,

    -- ** Mapping over lines of text
    --
    -- Functions of the type described here are used to perform statelses updates on a buffer, for
    -- example a context-free search and replace function. It is possible to halt a mapping function
    -- by evaluating a halting continuation function provided by 'forLinesInRange', 'forLines', and
    -- 'forLinesInBuffer'.

    MapLines, mapLines, mapAllLines, mapLinesBetween, mapLinesInRange, runMapLinesStep,

    -- ** A Function Type for Both Folding and Mapping

    FoldMapLines, foldMapLines, foldMapAllLines, foldMapLinesBetween, foldMapLinesInRange,
    runFoldMapLinesStep,

    -- ** Folding and mapping over characters

    FoldMapChars, foldMapChars, runFoldMapChars, execFoldMapChars,

    -- ** Mapping over characters in a line of text
    --
    -- The 'MapChars' function type is a special case of 'FoldMapChars'.

    MapChars, runMapChars,

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
                 ( GapBuffer, GapBufferState, gapBufferLength, pushSlice,
                   runGapBuffer, runGapBufferNew, cloneGapBufferState, freezeVector,
                   gapBufferCursorIsDefined, gapBufferBeforeCursor, gapBufferAfterCursor, 
                   IIBuffer, IIBufferState(IIBufferState, theIIBufferVector, theIIBufferCursor),
                   runIIBuffer, iiBufferVector, iiBufferCursor, gapBufferLiftIIBuffer,
                   VecIndex, VecLength(VecLength), indexAfterRange,
                   Absolute(Absolute), Relative(Relative), relative, unwrapRelative,
                   RelativeToCursor(Before, After), BufferError(..),
                   HasOpposite(opposite), HasNullValue(..),
                   IsIndex(fromIndex, toIndex),
                   IsLength(fromLength, toLength), distanceFromOrigin,
                   MonadVectorCursor(modCount, getElemIndex, shiftCursor, getAllocSize),
                   countDefined, relativeIndex, forceCursorIndex, writingIndex,
                   moveCursorBy, moveCursorTo, moveCursorNear,
                   deleteFromEnd, insertElemAtEnd, putElemIndex,
                   getElem, putElem, pushElem, popElem, pushElemVec,
                   indexToAbsolute, absoluteIndex, testIndex, validateIndex,
                   withFullSlices, sliceFromCursor, getSlice, copyRegion,
                   UnsafeSlice, sliceSize, readSlice, writeSlice, withRegion,
                   sliceIndiciesReverse, sliceIndiciesForward, safeClone, safeFreeze, safeCopy,
                 )
import           Hakshell.String
                 ( IntSized(intSize), Unpackable(unpack), Packable(pack),
                   StrictBytes, CharVector,
                 )
import           Hakshell.TextEditor.LineBreaker
                 ( LineBreakSymbol(..), LineBreaker(theLineBreaker), defaultLineBreakSymbol,
                   lineBreaker, lineBreakSize, lineBreakerNLCR, lineBreakPredicate
                 )

import           Control.Arrow ((|||))
import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Lens ((<&>), (%~), (.~), (^.), (+~), (.=), (&), Lens', lens, use, assign)
import           Control.Monad ((>=>), join, guard, when, unless, void)
import           Control.Monad.Cont (ContT(..))
import           Control.Monad.Cont.Class (MonadCont(..))
import           Control.Monad.Except (ExceptT(..), runExceptT)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.Reader (ReaderT(runReaderT))
import           Control.Monad.Reader.Class (MonadReader(ask))
import           Control.Monad.ST (runST)
import           Control.Monad.State (StateT(..), evalStateT, evalState, gets, modify)
import           Control.Monad.State.Class (MonadState(state, get, put))
import           Control.Monad.Trans.Class (MonadTrans(lift))

import           Data.Functor.Identity (Identity(..))
import           Data.Monoid ((<>))
import qualified Data.Primitive.MutVar       as Var
import qualified Data.Vector                 as Vec
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
newtype CharCount = CharCount Int
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

unwrapCharCount :: CharCount -> Int
unwrapCharCount (CharCount o) = o

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
    { cursorStepCount :: !(Relative CharIndex)
      -- ^ When an 'TextBuffer' stateful operation produces a value of this type, it is an
      -- indication of the minimum number of cursor movements that would have had to been made by an
      -- end user pressing keyboard keys (e.g. pressing the right-arrow to perform a traversal) in
      -- order to replicate the stateful operation. The biggest difference between this number and
      -- the 'deltaCharCount' is that line breaks are always considered a single cursor step,
      -- regardless of how many characters comprise the line break.
    , deltaCharCount  :: !CharCount
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

instance HasOpposite CharStats where
  opposite (CharStats{cursorStepCount=csc,deltaCharCount=dcc}) =
    CharStats
    { cursorStepCount = negate csc
    , deltaCharCount  = negate dcc
    }

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
  :: (Bounded i, IsIndex i, IsLength len, MonadVectorCursor vec m)
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

bufferToTextEditError :: MonadVectorCursor vec m => BufferError -> m TextEditError
bufferToTextEditError = bufferToEditorError EndOfLineBuffer LineIndexOutOfRange LineCountOutOfRange

bufferToLineEditError :: MonadVectorCursor vec m => BufferError -> m TextEditError
bufferToLineEditError = bufferToEditorError EndOfCharBuffer CharIndexOutOfRange CharCountOutOfRange

liftMutableGapBuffer
  :: (Monad m, Monad lifted, Monad prim,
      MonadVectorCursor vec (GapBuffer vec prim)
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

instance HasNullValue (TextLine tags) where { nullValue = TextLineUndefined; }

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

-- | The 'CharCount' of a 'TextLine' is essentially the number of steps it would take for an
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
textLineCursorSpan :: TextLine tags -> CharCount
textLineCursorSpan = CharCount . \ case
  TextLineUndefined -> error "textLineCursorSpan: undefined line"
  TextLine{theTextLineString=vec} -> UVec.length vec

-- | Return 'CharStats' statistics about this 'TextLine'.
textLineStats :: TextLine tags -> CharStats
textLineStats = \ case
  TextLineUndefined -> mempty
  TextLine{theTextLineString=vec,theTextLineBreakSymbol=lbrk} ->
    let len = UVec.length vec in
    CharStats
    { cursorStepCount = Relative $ CharIndex len
    , deltaCharCount = CharCount $ len +
        if lbrk == NoLineBreak then 0 else 1 - lineBreakSize lbrk
    }

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

-- | This is a reference to the stateful data of your buffer of text. The type variable is a monad
-- @m@ in which the 'TextBuffer' will be evaluated, typically set to 'IO' or 'ST', althuogh any
-- monadic function type that instantiates the 'PrimMonad' class will work.
--
-- The text in the 'TextBuffer' is editable by evaluating any of the combinators that evaluate to an
-- 'EditText' function type. Declare a new 'TextBuffer' using the 'newTextBuffer' function, then
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
-- There is a separate function type in this module, 'EditLine', for editing the characters on a
-- single line of text which can be evaluated using the 'editLine' function. There are also batch
-- edting function types such as 'RewriteLines', 'FoldLines', 'MapLines', and 'FoldMapLines'.
newtype TextBuffer m tags = TextBuffer (Var.MutVar (PrimState m) (TextBufferState m tags))

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
--
-- The type @m@ must be the monad in which this data type has it's internal state updated, and it
-- must be a member of the typeclass 'PrimMonad'.
data TextBufferState m tags
  = TextBufferState
    { theBufferDefaultLine  :: !(TextLine tags)
      -- ^ The tag value to use when new 'TextLine's are automatically constructed after a line
      -- break character is inserted.
    , theBufferLineBreaker  :: LineBreaker
      -- ^ The function used to break strings into lines. This function is called every time a
      -- string is transferred from 'theBufferLineEditor' to to 'theLinesAbove' or 'theLinesBelow'.
    , theTextGapBufferState :: !(GapBufferState (MVec.MVector (PrimState m) (TextLine tags)))
      -- ^ Contains the gap buffer itself, which is defined in the "Hakshell.GapBuffer" module.
    , theBufferLineEditor   :: !(LineEditor m tags)
      -- ^ A data structure for editing individual characters in a line of text.
    , theBufferTargetCol    :: !(Absolute CharIndex)
      -- ^ When moving the cursor up and down the 'TextBuffer' (e.g. using 'gotoLocation'), the
      -- cursor should generally remain at the same character column location.
    }
  -- TODO: First do the TODO item on the 'TextBuffer' data type, then rename this to
  -- 'TextBuffer'. Create a 'runEditText' API function and export it.

-- Not for export: this is only used to set empty lines in the buffer.
bufferDefaultLine :: Lens' (TextBufferState st tags) (TextLine tags)
bufferDefaultLine = lens theBufferDefaultLine $ \ a b -> a{ theBufferDefaultLine = b }

-- | Entering a line-breaking character (e.g. @'\n'@) into a 'TextBufferState' using 'insertChar' or
-- 'insertString' results in several 'TextLine's being generated automatically. Whenever a
-- 'TextLine' is constructed, there needs to be a default tag value that is assigned to it. This
-- lens allows you to observe or set the default tag value.
bufferDefaultTags :: Lens' (TextBufferState st tags) tags
bufferDefaultTags = bufferDefaultLine . textLineTags

-- Not for export: access to the 'GapBufferState' within the 'TextBufferState' data structure.
textEditGapBuffer
  :: Lens' (TextBufferState m tags)
           (GapBufferState (MVec.MVector (PrimState m) (TextLine tags)))
textEditGapBuffer = lens theTextGapBufferState $ \ a b -> a{ theTextGapBufferState = b }

-- | The symbol used to break lines whenever the 'lineBreak' is used. Changing this value does not
-- convert the line break symbols used anywhere else in the buffer, only the symbol that is to be
-- used from now on.
bufferLineBreaker :: Lens' (TextBufferState m tags) LineBreaker
bufferLineBreaker = lens theBufferLineBreaker $ \ a b -> a{ theBufferLineBreaker = b }

-- Not for export: indicates whether the line under the cursor is defined or undefined.
lineCursorIsDefined :: Lens' (TextBufferState m tags) Bool
lineCursorIsDefined = textEditGapBuffer . gapBufferCursorIsDefined

-- | The current line of text being edited under the cursor.
bufferLineEditor :: Lens' (TextBufferState m tags) (LineEditor m tags)
bufferLineEditor = lens theBufferLineEditor $ \ a b -> a{ theBufferLineEditor = b }

-- | When moving the cursor up and down the 'TextBuffer' (e.g. using 'gotoLocation'), the cursor
-- should generally remain at the same character column location.
bufferTargetCol :: Lens' (TextBufferState m tags) (Absolute CharIndex)
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
--
-- The type @m@ must be the monad in which this data type has it's internal state updated, and it
-- must be a member of the typeclass 'PrimMonad'.
data LineEditor m tags
  = LineEditor
    { theParentTextEditor  :: TextBuffer m tags
      -- ^ A line editor can be removed from it's parent 'TextEditor'. A 'LineEditor' is defined
      -- such that it can only directly edit text in it's parent 'TextEditor'. A 'LineEditor' can
      -- indirectly edit text in any other parent, but only if a complete copy of the content of the
      -- line editor is made and passed it to some other 'TextEditor'.
    , theLineEditGapBuffer   :: !(GapBufferState (UMVec.MVector (PrimState m) Char))
      -- ^ The internal 'GapBufferState'.
    , theLineEditorIsClean :: !Bool
      -- ^ This is set to 'True' if 'theBufferLineEditor' is a perfect copy of the 'TextLine'
      -- currently being referred to by the 'getLineNumber'. If the content of
      -- 'theBufferLineEditor' has been modified by 'insertChar' or 'deleteChars', or if cursor has
      -- been moved to a different line, this field is set to 'False'.
    , theLineBreakSymbol   :: !LineBreakSymbol
    , theLineEditorTags    :: tags
    }

instance PrimMonad m => IntSized (LineEditor m tags) where { intSize = lineEditorCharCount; }

parentTextEditor :: Lens' (LineEditor m tags) (TextBuffer m tags)
parentTextEditor = lens theParentTextEditor $ \ a b -> a{ theParentTextEditor = b }

lineEditGapBuffer :: Lens' (LineEditor m tags) (GapBufferState (UMVec.MVector (PrimState m) Char))
lineEditGapBuffer = lens theLineEditGapBuffer $ \ a b -> a{ theLineEditGapBuffer = b }

-- Not for export: this gets updated when inserting or deleting characters before the cursor.
charsBeforeCursor :: Lens' (LineEditor m tags) VecLength
charsBeforeCursor = lineEditGapBuffer . gapBufferBeforeCursor

-- Not for export: this gets updated when inserting or deleting characters after the cursor.
charsAfterCursor :: Lens' (LineEditor m tags) VecLength
charsAfterCursor = lineEditGapBuffer . gapBufferAfterCursor

-- Not for export: this can put the 'LineEditor' into an inconsistent state. Use
-- 'setLineBreakSymbol' instead.
lineBreakSymbol :: Lens' (LineEditor m tags) LineBreakSymbol
lineBreakSymbol = lens theLineBreakSymbol $ \ a b -> a{ theLineBreakSymbol = b }

-- | A 'Control.Lens.Lens' to get or set tags for the line currently under the cursor. To use or
-- modify the tags value of the line under the cursor, evaluate one of the functions 'use',
-- 'modifying', @('Control.Lens..=')@, or @('Control.Lens.%=')@ within an 'EditText' function, or
-- any function which instantiates 'MonadEditText'.
lineEditorTags :: Lens' (LineEditor m tags) tags
lineEditorTags = lens theLineEditorTags $ \ a b -> a{ theLineEditorTags = b }

-- | This is set to 'True' if 'theBufferLineEditor' is a perfect copy of the 'TextLine' currently
-- being referred to by the 'getLineNumber'. If the content of 'theBufferLineEditor' has been
-- modified by 'insertChar' or 'deleteChars', or if cursor has been moved to a different line, this
-- field is set to 'False'.
lineEditorIsClean :: Lens' (LineEditor m tags) Bool
lineEditorIsClean = lens theLineEditorIsClean $ \ a b -> a{ theLineEditorIsClean = b }

-- This is set to 'True' if the element under the cursor is defined, 'False' if the element under
-- the cursor is undefined.
charCursorIsDefined :: Lens' (LineEditor m tags) Bool
charCursorIsDefined = lineEditGapBuffer . gapBufferCursorIsDefined

----------------------------------------------------------------------------------------------------

-- | This is a type of functions that can modify the textual content stored in a 'TextBufferState'.
newtype EditText tags m a
  = EditText (
      ReaderT (TextBuffer m tags)
        (ExceptT TextEditError (StateT (TextBufferState m tags) m)) a
    )
  deriving
    ( Functor, Applicative, Monad, MonadIO,
      MonadError TextEditError
    )

--instance Monad m => MonadState (TextBufferState tags) (EditText tags m) where
--  state = EditText . lift . state
--
--instance Monad m => MonadError TextEditError (EditText tags m) where
--  throwError = EditText . throwError
--  catchError (EditText try) catch = EditText $ catchError try $ unwrapEditText . catch

instance MonadTrans (EditText tags) where
  lift = EditText . lift . lift . lift

instance (Monad m, Semigroup a) => Semigroup (EditText tags m a) where
  a <> b = (<>) <$> a <*> b

instance (Monad m, Monoid a) => Monoid (EditText tags m a) where
  mempty = return mempty
  mappend a b = mappend <$> a <*> b

editTextState
  :: PrimMonad m
  => StateT (TextBufferState m tags) m a
  -> EditText tags m a
editTextState = EditText . lift . lift

-- | Evaluate an 'EditText' function on the given 'TextBufferState'. The given 'EditText' function
-- is evaluates all updates on the given 'TextBuffer' atomically and in-place (i.e. without copying
-- anything unless instructed to do so). This is the most efficient way to evaluate an 'EditText'
-- function, but is more restrictive in that it can only be evaluated when the 'EditText' data
-- type's @m@ parameter is set to the @IO@ type, meaning you cannot use this function if the
-- 'EditText's @m@ parameter is something other than @IO@, like for example a 'ReaderT' type.
runEditText
  :: (PrimMonad m, st ~ PrimState m)
  => EditText tags m a
  -> TextBuffer m tags
  -> m (Either TextEditError a)
runEditText (EditText f) this@(TextBuffer mvar) = do
  (result, buffer) <- Var.readMutVar mvar >>=
    runStateT (runExceptT $ runReaderT f this)
  Var.writeMutVar mvar buffer
  return result

-- | Evaluate an 'EditText' function on the given 'TextBufferState', but unlike 'runEditTextIO', a
-- copy of the entire text buffer is first created, and all updates are performed on the copy
-- atomically. This function is less restrictive than 'runEditTextIO' because it will work for
-- 'EditText' functions where the @m@ parameter is not just @IO@ but any member of the 'Monad'
-- typeclass, however the trade-off is that a copy of the text buffer must be made.
runEditTextOnCopy
  :: PrimMonad m
  => EditText tags m a
  -> TextBuffer m tags
  -> m (Either TextEditError a, TextBuffer m tags)
runEditTextOnCopy (EditText f) = copyTextBuffer >=> \ copy@(TextBuffer mvar) -> do
  (result, st) <- Var.readMutVar mvar >>= runStateT (runExceptT $ runReaderT f copy)
  Var.writeMutVar mvar st
  return (result, copy)

-- Extracts 'textEditGapBuffer', uses it to evaluate 'runGapBuffer', then after the 'runGapBuffer'
-- evaluation completes (which may have updated the 'GapBuffer'), replaces the updated
-- 'textEditGapBuffer'.
editTextLiftGapBuffer
  :: (PrimMonad m, st ~ PrimState m)
  => GapBuffer (MVec.MVector st (TextLine tags)) m a
  -> EditText tags m a
editTextLiftGapBuffer =
  liftMutableGapBuffer textEditGapBuffer bufferToTextEditError id (EditText . lift)

-- | Like 'textLineIndex', but throws a 'LineIndexOutOfRange' exception if the given 'LineIndex' is
-- out of bounds.
validateLineIndex
  :: PrimMonad m
  => Absolute LineIndex
  -> EditText tags m (Absolute LineIndex)
validateLineIndex = editTextLiftGapBuffer . (validateIndex >=> indexToAbsolute)

-- | Like 'validateLineIndex', but rather than throwing an exception, simply returns 'False' if an
-- exception would have been thrown, and returns 'True' otherwise.
lineIndexIsValid :: PrimMonad m => Absolute LineIndex -> EditText tags m Bool
lineIndexIsValid = editTextLiftGapBuffer . fmap fst . testIndex

-- | Evaluates 'validateLineIndex' on a given 'LineIndex', and if no exceptions are thrown, also
-- dereferences the index, returning the 'TextLine' stored at the given normalized logical index.
validateGetLineIndex
  :: PrimMonad m
  => Absolute LineIndex
  -> EditText tags m (Absolute LineIndex, TextLine tags)
validateGetLineIndex = editTextLiftGapBuffer .
  (validateIndex >=> \ i -> (,) <$> indexToAbsolute i <*> getElemIndex i)

----------------------------------------------------------------------------------------------------

-- | Functions of this type operate on a single 'TextLine', it is useful for updating the
-- 'bufferLineEditor'.
newtype EditLine tags m a
  = EditLine
    ( ExceptT TextEditError
        (StateT (LineEditor m tags) (EditText tags m)) a
    )
  deriving
    ( Functor, Applicative, Monad, MonadIO,
      MonadError TextEditError
    )
  -- TODO: try lifting @m@ instead of @(EditText tags m)@

--instance Monad m => MonadState (LineEditor mvar st tags) (EditLine mvar tags m) where
--  state = EditLine . lift . state
--
--instance Monad m =>  MonadError TextEditError (EditLine mvar tags m) where
--  throwError = EditLine . throwError
--  catchError (EditLine try) catch = EditLine $ catchError try $ unwrapEditLine . catch

instance MonadTrans (EditLine tags) where
  lift = EditLine . lift . lift . lift

class MonadEditLine editor where
  -- | Perform an edit on the line under the cursor. You usually call this function after
  -- positioning the cursor within a 'TextBuffer' using a function like 'gotoLine', then edit the
  -- characters on thatline by evaluating 'EditLine' functinos using this function 'editLine'. For
  -- example:
  --
  -- >>> 'gotoLine' 4
  --     'editLine' $ do
  --         'gotoChar' 'maxBound'
  --         'insertString' " This is written to the end of the line."
  editLine
    :: (Monad (editor m), PrimMonad m, EditorTagsType (editor m) ~ tags)
    => EditLine tags m a -> editor m a

instance MonadEditLine (EditText tags) where
  editLine (EditLine f) = editTextState (use bufferLineEditor) >>=
    runStateT (runExceptT f) >>= \ case
      (Left err, _   ) -> throwError err
      (Right  a, line) -> editTextState (bufferLineEditor .= line) >> return a

editLineState
  :: PrimMonad m
  => StateT (LineEditor m tags) (EditText tags m) a
  -> EditLine tags m a
editLineState = EditLine . lift

-- Extracts 'textEditGapBuffer', uses it to evaluate 'runGapBuffer', then after the 'runGapBuffer'
-- evaluation completes (which may have updated the 'GapBuffer'), replaces the updated
-- 'textEditGapBuffer'.
editLineLiftGapBuffer
  :: (PrimMonad m, st ~ PrimState m)
  => GapBuffer (UMVec.MVector st Char) m a -> EditLine tags m a
editLineLiftGapBuffer =
  liftMutableGapBuffer lineEditGapBuffer bufferToLineEditError lift EditLine

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
  (PrimMonad m, st ~ PrimState m, tags ~ EditorTagsType (EditText tags m)
  ) => TextFoldable (EditText tags m) where
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
-- This function is used to define the algorithm used by several functions.
gFoldMapBetween ::
  ( MonadVectorCursor (vec (TextLine tags)) (gapperM m),
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
newtype MapLines tags r (m :: * -> *) a
  = MapLines{ unwrapMapLines :: ContT r (EditText tags m) a}
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadError TextEditError(MapLines tags r m) where
  throwError = MapLines . lift . throwError
  catchError (MapLines try) catch = MapLines $ ContT $ \ next ->
    catchError (runContT try next) $ \ err ->
    runContT (unwrapMapLines $ catch err) next

instance Monad m => MonadCont (MapLines tags r m) where
  callCC f = MapLines $ callCC $ unwrapMapLines . f . fmap MapLines
 
instance (Monad m, Monoid a) => Monoid (MapLines tags r m a) where
  mappend a b = mappend <$> a <*> b
  mempty = return mempty
 
instance (Monad m, Semigroup a) => Semigroup (MapLines tags r m a) where
  (<>) a b = (<>) <$> a <*> b

instance MonadTrans (MapLines tags r) where
  lift = MapLines . lift . lift

runMapLinesStep :: Monad m => MapLines tags a m a -> EditText tags m a
runMapLinesStep (MapLines f) = runContT f return

mapLinesBetween
  :: PrimMonad m
  => Absolute LineIndex
  -> Absolute LineIndex
  -> (MapLines tags () m void
      -> Absolute LineIndex
      -> TextLine tags
      -> MapLines tags () m (TextLine tags)
     )
  -> EditText tags m ()
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
  :: PrimMonad m
  => Absolute LineIndex
  -> Relative LineIndex
  -> (MapLines tags () m void
      -> Absolute LineIndex
      -> TextLine tags
      -> MapLines tags () m (TextLine tags)
     )
  -> EditText tags m ()
mapLinesInRange from rel f = if rel == 0 then return () else
  mapLinesBetween from (shiftAbsolute from rel) f

mapLines
  :: PrimMonad m
  => Relative LineIndex
  -> (MapLines tags () m void
      -> Absolute LineIndex
      -> TextLine tags
      -> MapLines tags () m (TextLine tags)
     )
  -> EditText tags m ()
mapLines rel f = getLineNumber >>= \ from -> mapLinesInRange from rel f

mapAllLines
  :: PrimMonad m
  => (MapLines tags () m void
      -> Absolute LineIndex
      -> TextLine tags
      -> MapLines tags () m (TextLine tags)
     )
  -> EditText tags m ()
mapAllLines = mapLinesBetween minBound maxBound

----------------------------------------------------------------------------------------------------

-- | A type of functions that can perform a fold (in the sense of the Haskell 'Control.Monad.foldM'
-- function) over lines of text in a 'TextBuffer' while also updating each line as they are
-- being folded. Each line of text is folded in place, and lines may not be removed or inserted (for
-- inserting or removing lines , use 'cursorLoop'). This is one of two methods of performing batch
-- operations on text, the other is 'RewriteLines'. The difference between 'FoldMapLines' functions
-- and 'RewriteLines' functions is that 'FoldMapLines' does not change the number of lines during
-- evaluation.
--
-- This function takes an arbitrary @fold@ data type which can be anything you want, and is
-- initialized when evaluating the 'runFoldMapLines' function. The 'FoldMapLines' function type
-- instantiates 'Control.Monad.State.Class.MonadState' over the @fold@ type, so you will use
-- 'Control.Monad.State.state', 'Control.Monad.State.modify', 'Control.Monad.State.get', and
-- 'Control.Monad.State.put' functions.
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
  deriving (Functor, Applicative, Monad)

type instance EditorTagsType (FoldMapLines tags fold r m) = tags

instance Monad m => MonadState fold (FoldMapLines tags fold r m) where
  state = FoldMapLines . lift . state

instance Monad m => MonadError TextEditError (FoldMapLines tags fold r m) where
  throwError = FoldMapLines . lift . throwError
  catchError (FoldMapLines try) catch = FoldMapLines $ ContT $ \ next ->
    catchError (runContT try next) $ flip runContT next . unwrapFoldMapLines . catch

instance Monad m => MonadCont (FoldMapLines tags fold r m) where
  callCC f = FoldMapLines $ callCC $ unwrapFoldMapLines . f . (FoldMapLines .)

instance MonadTrans (FoldMapLines tags fold r) where
  lift = foldMapLiftEditText . lift

instance (Monad m, Semigroup a) => Semigroup (FoldMapLines r fold tags m a) where
  a <> b = (<>) <$> a <*> b

instance (Monad m, Monoid a) => Monoid (FoldMapLines r fold tags m a) where
  mappend a b = mappend <$> a <*> b
  mempty = return mempty

foldMapLiftEditText :: Monad m => EditText tags m a -> FoldMapLines r fold tags m a
foldMapLiftEditText = FoldMapLines . lift . lift . lift

-- | Convert a 'FoldMapLines' into an 'EditText' function. This function is analogous to the
-- 'runStateT' function. This function does not actually perform a fold or map operation, rather it
-- simply unwraps the 'EditText' monad that exists within the 'FoldMapLines' monad.
runFoldMapLinesStep
  :: Monad m
  => FoldMapLines r fold tags m a
  -> (a -> EditText tags m r)
  -> fold -> EditText tags m (r, fold)
runFoldMapLinesStep (FoldMapLines f) end =
  runStateT (runExceptT $ runContT f $ lift . lift . end) >=> \ case
    (Left err, _   ) -> throwError err
    (Right  a, fold) -> return (a, fold)

foldMapLinesBetween
  :: PrimMonad m
  => Absolute LineIndex
  -> Absolute LineIndex
  -> fold
  -> (FoldMapLines () fold tags m void
      -> Absolute LineIndex
      -> TextLine tags
      -> FoldMapLines () fold tags m (TextLine tags)
     )
  -> EditText tags m fold
foldMapLinesBetween = gFoldMapBetween
  lift
  editTextLiftGapBuffer
  (\ f -> runFoldMapLinesStep f $ const $ return ())
  (readSlice MVec.read)
  writeSlice
  (sliceSize MVec.length)

foldMapLinesInRange
  :: PrimMonad m
  => Absolute LineIndex
  -> Relative LineIndex
  -> fold
  -> (FoldMapLines () fold tags m void
      -> Absolute LineIndex
      -> TextLine tags
      -> FoldMapLines () fold tags m (TextLine tags)
     )
  -> EditText tags m fold
foldMapLinesInRange from rel fold f = if rel == 0 then return fold else
  foldMapLinesBetween from (shiftAbsolute from rel) fold f

foldMapLines
  :: PrimMonad m
  => Relative LineIndex
  -> fold
  -> (FoldMapLines () fold tags m void
      -> Absolute LineIndex
      -> TextLine tags
      -> FoldMapLines () fold tags m (TextLine tags)
     )
  -> EditText tags m fold
foldMapLines rel fold f = getLineNumber >>= \ from -> foldMapLinesInRange from rel fold f

foldMapAllLines
  :: PrimMonad m
  => fold
  -> (FoldMapLines () fold tags m void
      -> Absolute LineIndex
      -> TextLine tags
      -> FoldMapLines () fold tags m (TextLine tags)
     )
  -> EditText tags m fold
foldMapAllLines = foldMapLinesBetween minBound maxBound

----------------------------------------------------------------------------------------------------

-- | A type of functions that can perform a batch rewrite operation over lines of text in a
-- 'TextBuffer'. Each line of text popped from the 'TextBuffer' and passed to the continuation
-- function. Lines may be pushed back using 'writeBack', and you can push back as many lines as you
-- choose -- not calling 'writeBack' deletes the line. This is one of two methods of performing
-- batch operations on text, the other is 'RewriteLines'. The difference between 'FoldMapLines'
-- functions and 'RewriteLines' functions is that 'FoldMapLines' does not change the number of lines
-- during evaluation.
--
-- The 'RewriteLine' function evaluation context contains it's own 'LineEditor' which is separate
-- from the 'LineEditor' of the 'EditText' evaluation context, so it does not interfear with the
-- line of text currently being edited while the batch operation is running, although when the batch
-- operation completes the text around the line being currently edited may be completely altered.
--
-- Like the 'FoldMapLines' function, the 'RewriteLine' function take an arbitrary @fold@ data type,
-- which can be altered using the 'Control.Monad.State.Class.MonadState' function instances over the
-- @fold@ type, so you will use 'Control.Monad.State.state', 'Control.Monad.State.modify',
-- 'Control.Monad.State.get', and 'Control.Monad.State.put' functions.
newtype RewriteLines fold tags m a
  = RewriteLines
    { unwrapRewriteLines ::
      ContT fold
      (ExceptT TextEditError
        (StateT
          (RewriteLinesState fold tags m)
          (EditText tags m)
        )
      ) a
    }
  deriving (Functor, Applicative, Monad, MonadIO)

type instance EditorTagsType (RewriteLines fold tags m) = tags

data RewriteLinesState fold tags m
  = RewriteLinesState
    { theRewriteLinesDirection :: !RelativeToCursor
    , theRewriteLinesFold :: !fold
    , theRewriteLinesEditor :: !(LineEditor m tags)
    }

instance Monad m => MonadState fold (RewriteLines fold tags m) where
  state f = RewriteLines $
    lift $ lift $ state $ \ rwlst ->
    let (a, st) = f (theRewriteLinesFold rwlst) in
    (a, rwlst{ theRewriteLinesFold = st })

instance Monad m => MonadError TextEditError (RewriteLines fold tags m) where
  throwError = RewriteLines . lift . throwError
  catchError (RewriteLines (ContT try)) catch =
    RewriteLines $ ContT $ \ next ->
    catchError (try next) $
    flip runContT next . unwrapRewriteLines . catch

instance Monad m => MonadCont (RewriteLines fold tags m) where
  callCC f = RewriteLines $ callCC $ unwrapRewriteLines . f . fmap RewriteLines

instance MonadTrans (RewriteLines fold tags) where
  lift = RewriteLines . lift . lift . lift . lift

instance MonadEditLine (RewriteLines fold tags) where
  editLine  (EditLine f) =
    ( RewriteLines $
      lift $ lift $
      gets theRewriteLinesEditor >>=
      lift . runStateT (runExceptT f)
    ) >>= \ case
      (Left err, _   ) -> throwError err
      (Right  a, line) ->
        rewriteLinesState (rewriteLinesEditor .= line) >>
        return a

rewriteLinesEditor :: Lens' (RewriteLinesState fold tags m) (LineEditor m tags)
rewriteLinesEditor = lens theRewriteLinesEditor $ \ a b -> a{ theRewriteLinesEditor = b }

rewriteLinesLiftEditText :: Monad m => EditText tags m a -> RewriteLines fold tags m a
rewriteLinesLiftEditText = RewriteLines . lift . lift . lift

rewriteLinesState
  :: Monad m
  => StateT (RewriteLinesState fold tags m) (EditText tags m) a
  -> RewriteLines fold tags m a
rewriteLinesState = RewriteLines . lift . lift

-- not for export, performs no bounds checking.
rewriteLinesLoop
  :: PrimMonad m
  => fold
  -> (RewriteLines fold tags m void
       -> TextLine tags
       -> RewriteLines fold tags m ()
     )
  -> Int -> RelativeToCursor -> EditText tags m fold
rewriteLinesLoop fold f count direction =
  editTextState (use bufferDefaultTags) >>=
  lift . newLineEditor Nothing >>= \ lineEd ->
  ( evalStateT
    ( runExceptT $
      runContT
      (unwrapRewriteLines (callCC $ loop count))
      (const $ gets theRewriteLinesFold)
    )
    RewriteLinesState
    { theRewriteLinesDirection = direction
    , theRewriteLinesFold = fold
    , theRewriteLinesEditor = lineEd
    }
  ) >>=
  (throwError ||| return)
  where
    loop count halt =
      if count <= 0 then get else
      rewriteLinesState (gets theRewriteLinesDirection) >>= \ dir ->
      rewriteLinesLiftEditText (popLine dir) >>=
      f (get >>= halt) >>
      (loop $! count - 1) halt

-- | The 'rewriteLines' function works by calling 'popLine' repeatedly, and passing the 'TextLine'
-- returned by 'popLine' to the loop continuation. This essentially deletes every line in the range
-- of lines over which the 'rewriteLines' loop is evaluated. To prevent a line from being deleted,
-- this 'writeBack' function __must__ be evaluated before each iteration terminates. It is possible
-- to push back any line at all, and any number of lines, this is what allows lines to be rewritten
-- by the 'rewriteLines' loop.
writeBack :: PrimMonad m => TextLine tags -> RewriteLines fold tags m ()
writeBack line =
  rewriteLinesState (gets theRewriteLinesDirection) >>=
  rewriteLinesLiftEditText . flip pushLine line . opposite

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
  :: (PrimMonad m
     , Show tags --DEBUG
     )
  => Absolute LineIndex -> Absolute LineIndex
  -> fold
  -> (RewriteLines fold tags m void
      -> TextLine tags
      -> RewriteLines fold tags m ()
     )
  -> EditText tags m fold
rewriteLinesInRange from to fold f = do
  gotoLine from
  rewriteLinesLoop fold f (max 1 $ abs $ fromLength $ diffAbsolute from to) $
    if from < to then After else Before

-- | Conveniently calls 'forLinesInRange' with the first two parameters as @('Absolute' 1)@ and
-- @('Absolute' 'maxBound')@.
rewriteLinesInBuffer
  :: (PrimMonad m
     , Show tags --DEBUG
     )
  => fold
  -> (RewriteLines fold tags m  void
      -> TextLine tags
      -> RewriteLines fold tags m ()
     )
  -> EditText tags m fold
rewriteLinesInBuffer = rewriteLinesInRange (Absolute 1) maxBound

-- | Like 'rewriteLinesInRange', but this function takes a 'RelativeToCursor' value, iteration
-- begins at the cursor position where the 'bufferLineEditor' is set, and if the 'RelativeToCursor'
-- value is 'After' then iteration goes forward to the end of the buffer, whereas if the
-- 'RelativeToCursor' value is 'Before' then iteration goes backward to the start of the buffer.
rewriteLines
  :: ( PrimMonad m
     , Show tags --DEBUG
     )
  => RelativeToCursor
  -> fold
  -> (RewriteLines fold tags m void
      -> TextLine tags
      -> RewriteLines fold tags m ()
     )
  -> EditText tags m fold
rewriteLines rel fold f = do
  linum <- getLineNumber
  count <- bufferLineCount
  uncurry (rewriteLinesLoop fold f . max 1) $ case rel of
    Before -> (fromIndex linum, Before)
    After  -> (fromLength count - fromIndex linum, After)

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
cursorIsOnLineBreak =
  indexIsOnLineBreak <$> use viewerLine <*> viewLineLiftIIBuffer (writingIndex Before)

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
  :: (PrimMonad m, st ~ PrimState m)
  => TextLocation
  -> EditText tags m (Either TextLocation (TextLine tags, (Bool, TextLocation)))
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
  :: (PrimMonad m, st ~ PrimState m)
  => TextLocation -> EditText tags m (TextLocation, TextLine tags)
validateLocation loc0 = testLocation loc0 >>= \ case
  Left{}  -> throwError $ LineIndexOutOfRange $ loc0 ^. lineIndex
  Right (textln, (ok, loc)) -> if ok then return (loc, textln) else
    throwError $ CharIndexOutOfRange $ loc0 ^. charIndex

-- | Like 'validateLocation' but rather than throwing an exception, returns 'Nothing'. If the
-- location is valid, the 'TextLine' at the valid 'TextLocation' is returned as a 'Just' value.
locationIsValid
  :: (PrimMonad m, st ~ PrimState m)
  => TextLocation -> EditText tags m (Maybe (TextLine tags))
locationIsValid = (`catchError` (const $ return Nothing)) . fmap (Just . snd) . validateLocation

-- | This predicate function checks if an @('Absolute' 'CharIndex')@ points to the line breaking
-- character beyond the last character in a 'TextLine'. When single-stepping a 'TextLocation' cursor
-- such that you evaluate 'locationIsValid' after every step, it is important to evaluate this
-- function on the resultant 'TextLine', and if this function evaluates to 'True' you must increment
-- the 'lineIndex' and reset the 'charIndex' to @1@.
indexIsOnLineBreak :: TextLine tags -> VecIndex -> Bool
indexIsOnLineBreak line = ((unwrapCharCount $ textLineCursorSpan line) ==) . fromIndex

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
  :: (PrimMonad m, st ~ PrimState m)
  => Maybe VecLength -> tags -> m (TextBuffer m tags)
newTextBuffer initSize tags = do
  cur <- UMVec.new defaultInitLineBufferSize >>=
    lineEditorFromVector tags (pure ())
  MVec.replicate (maybe defaultInitTextBufferSize fromLength initSize) TextLineUndefined >>=
    textBufferFromVector tags cur (pure ())

-- not for export
textBufferFromVector
  :: (PrimMonad m, st ~ PrimState m)
  => tags
  -> LineEditor m tags
  -> GapBuffer (MVec.MVector st (TextLine tags)) m ()
  -> MVec.MVector st (TextLine tags)
  -> m (TextBuffer m tags)
textBufferFromVector tags cur initVec mvec = do
  mvar <- textBufferStateFromVector tags cur initVec mvec >>= Var.newMutVar
  let this = TextBuffer mvar
  Var.modifyMutVar mvar $ bufferLineEditor . parentTextEditor .~ this
  return this

-- | Create a deep-copy of a 'TextBuffer'. Everything is copied perfectly, including the cursor
-- location, and the content and state of the cursor.
copyTextBuffer
  :: (PrimMonad m, st ~ PrimState m)
  => TextBuffer m tags -> m (TextBuffer m tags)
copyTextBuffer (TextBuffer mvar) = do
  oldst      <- Var.readMutVar mvar
  gbst       <- snd <$> runGapBuffer (cloneGapBufferState >>= put) (oldst ^. textEditGapBuffer)
  newLineBuf <- copyLineEditor (oldst ^. bufferLineEditor)
  mvar       <- Var.newMutVar oldst
    { theBufferLineEditor   = newLineBuf
    , theTextGapBufferState = gbst
    }
  let this = TextBuffer mvar
  Var.modifyMutVar mvar $ bufferLineEditor . parentTextEditor .~ this
  return this

-- not for export: returns an internal-use-only data type, also discards an exception from
-- runGapBufferNew.
textBufferStateFromVector
  :: (PrimMonad m, st ~ PrimState m)
  => tags -- ^ Specify the default @tags@ value.
  -> LineEditor m tags
  -> GapBuffer (MVec.MVector st (TextLine tags)) m ()
  -> MVec.MVector st (TextLine tags)
  -> m (TextBufferState m tags)
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
  :: (PrimMonad m, st ~ PrimState m)
  => Maybe VecLength -> tags -> m (LineEditor m tags)
newLineEditor initSize tag =
  UMVec.new (maybe defaultInitLineBufferSize fromLength initSize) >>=
  lineEditorFromVector tag (pure ())

lineEditorFromVector
  :: (PrimMonad m, st ~ PrimState m)
  => tags
  -> GapBuffer (UMVec.MVector st Char) m ()
  -> UMVec.MVector st Char
  -> m (LineEditor m tags)
lineEditorFromVector tag initBuf newBuf = do
  gbst <- snd <$> runGapBufferNew newBuf initBuf
  return LineEditor
    { theParentTextEditor  = error "internal: 'LineEditor{theParentTextEditor}' is undefined"
    , theLineEditGapBuffer = gbst
    , theLineEditorIsClean = False
    , theLineBreakSymbol = NoLineBreak
    , theLineEditorTags    = tag
    }  

-- Use this to create a deep-copy of a 'LineEditor'. The cursor location within the 'LineEditor'
-- is also copied.
copyLineEditor
  :: (PrimMonad m, st ~ PrimState m)
  => LineEditor m tags -> m (LineEditor m tags)
copyLineEditor cur = do
  gbst <- snd <$> runGapBuffer (cloneGapBufferState >>= put) (cur ^. lineEditGapBuffer)
  return cur{ theLineEditGapBuffer = gbst }

-- | A pure function you can use to determine how many characters have been stored into this
-- buffer. This function returns an 'Int' value because it is generally not to be used with other
-- functions in this module. The 'lineEditorLength' function returns a @'Relative' 'CharIndex'@ that
-- can be used with other functions.
lineEditorCharCount :: PrimMonad m => LineEditor m tags -> Int
lineEditorCharCount line = fromLength count + adjust where
  buf    = line ^. lineEditGapBuffer
  count  = buf ^. gapBufferBeforeCursor + buf ^. gapBufferAfterCursor
  adjust = case line ^. lineBreakSymbol of
    NoLineBreak -> 0
    sym         -> lineBreakSize sym - 1

-- | This funcion returns the same value as 'textLineCursorSpan' but for a 'LineEditor'.
lineEditorUnitCount :: PrimMonad m => LineEditor m tags -> Relative CharIndex
lineEditorUnitCount = relative . gapBufferLength . theLineEditGapBuffer

-- | Gets the 'lineEditorCharCount' value for the current 'LineEditor'.
getLineCharCount :: PrimMonad m => EditText tags m Int
getLineCharCount = lineEditorCharCount <$> editTextState (use bufferLineEditor)

-- | Gets the 'lineEditorUnitCount' value for the current 'LineEditor'.
getUnitCount :: PrimMonad m => EditText tags m (Relative CharIndex)
getUnitCount = lineEditorUnitCount <$> editTextState (use bufferLineEditor)

-- | Change the 'LineBreakSymbol' of the current 'LineEditor'.
setLineBreakSymbol :: PrimMonad m => LineBreakSymbol -> EditLine tags m ()
setLineBreakSymbol lbrk =
  getLineBreakSymbol >>= \ oldLbrk ->
  unless (lbrk == oldLbrk) $ do
    editLineState $ lineBreakSymbol .= lbrk
    editLineLiftGapBuffer $ case lbrk of
      NoLineBreak -> do
        count <- modCount After id
        unless (count == 0) $ deleteFromEnd (-1)
      _ -> case oldLbrk of
        NoLineBreak -> insertElemAtEnd After '\n'
        _ -> pure () -- there is a '\n' symbol at the end already

-- | Pop a 'TextLine' from before or after the cursor. This function does not effect the content of
-- the 'bufferLineEditor'. If you 'popLine' from 'Before' the cursor when the 'bufferLineEditor' is
-- at the beginning of the buffer, or if you 'popLine' from 'After' the cursor when the
-- 'bufferLineEditor' is at the end of the buffer, this function evaluates to an 'EndOfLineBuffer'
-- exception.
popLine :: (PrimMonad m, st ~ PrimState m) => RelativeToCursor -> EditText tags m (TextLine tags)
popLine = editTextLiftGapBuffer . popElem

-- | Push a 'TextLine' before or after the cursor without modifying the content of the 'LineEditor'.
pushLine
  :: (PrimMonad m, st ~ PrimState m)
  => RelativeToCursor -> TextLine tags -> EditText tags m ()
pushLine rel = editTextLiftGapBuffer . pushElem rel

-- TODO: uncomment this, rewrite it
--
---- | Create a new 'LineEditor' by copying the line under the given 'TextLocation' point. The
---- 'LineEditor' can be updated with an 'EditLine' function. Note that this function works in any
---- monadic function type @m@ which instantiates 'Control.Monad.IO.Class.MonadIO, so this will work
---- in the @IO@ monad, the 'EditText' monad, the 'EditLine' monad, and in other contexts as well.
--newLineEditorAt
--  :: PrimMonad m
--  => TextBuffer mvar tags -> TextLocation -> m (Either TextEditError (LineEditor mvar st tags))
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

-- | Insert a 'TextLine' into the current 'LineEditor'. If the 'TextLine' is not terminated with a
-- line breaking symbol (the 'textLineBreakSymbol' is 'NoLineBreak') the content of the given
-- 'TextLine' is simply copied to the current 'LineEditor', then the content of the 'TextLine' is
-- placed 'Before' or 'After' the cursor.
--
-- If the 'textLineBreakSymbol' is not 'NoLineBreak', then the 'RelativeToCursor' parameter is
-- ignored, the content of the 'TextLine' is appended to the content in the 'LineEditor' that exists
-- before the cursor, then the resultant 'TextLine' is pushed to the 'TextBuffer' before the cursor,
-- leaving the cursor at the start of the line.
insertTextLine :: PrimMonad m => RelativeToCursor -> TextLine tags -> EditText tags m ()
insertTextLine rel = \ case
  TextLineUndefined -> error $
    "internal error: evaluated (insertTextLine TextLineUndefined)"
  line              -> case line ^. textLineBreakSymbol of
    NoLineBreak       ->
      editLine $
      editLineLiftGapBuffer $
      pushElemVec rel $
      theTextLineString line
    lbrk              ->
      ( editLine $
        editLineState (use lineEditorTags) >>= \ tags ->
        editLineLiftGapBuffer $ do
          pushElemVec Before $ theTextLineString line
          vec <- withFullSlices $ \ before _after -> safeFreeze before
          modCount Before $ const 0
          return TextLine
            { theTextLineString = vec
            , theTextLineBreakSymbol = lbrk
            , theTextLineTags = tags
            }
      ) >>=
      editTextLiftGapBuffer . pushElem Before

-- Not for export: I don't want to encourage the use of a self-reference within an 'EditText'
-- function. It is already easy enough to cause a deadlock by evaluating 'runEditText' with 'liftIO'
-- within a 'runEditText' function.
--
-- Obtain a reference to the 'TextBuffer' that this was given to this function's monadic evaluator.
thisTextBuffer :: Monad m => EditText tags m (TextBuffer m tags)
thisTextBuffer = EditText ask

-- | Create a copy of the 'bufferLineEditor'.
copyLineEditorText :: (PrimMonad m, st ~ PrimState m) => EditLine tags m (TextLine tags)
copyLineEditorText = TextLine
  <$> editLineLiftGapBuffer freezeVector
  <*> editLineState (use lineBreakSymbol)
  <*> editLineState (use lineEditorTags)

-- TODO: make this more abstract, make it callable from within any 'CursorIndexedLine' monad.
validateRelativeChar :: PrimMonad m => Relative CharIndex -> EditLine tags m VecIndex
validateRelativeChar rel =
  flip shiftAbsolute rel <$> getColumnNumber >>= editLineLiftGapBuffer . validateIndex

-- not for export
makeLineWithSlice
  :: PrimMonad m
  => (LineBreakSymbol -> LineBreakSymbol) -> CharVector -> EditLine tags m (TextLine tags)
makeLineWithSlice onLbrk slice = do
  lbrk  <- editLineState $ use lineBreakSymbol
  tags  <- editLineState $ use lineEditorTags
  return $ textLineBreakSymbol %~ onLbrk $ TextLine
    { theTextLineString      = slice
    , theTextLineTags        = tags
    , theTextLineBreakSymbol = lbrk
    }

-- | Create a 'TextLine' by copying the characters relative to the cursor.
copyChars :: PrimMonad m => Relative CharIndex -> EditLine tags m (TextLine tags)
copyChars rel = do
  break <- validateRelativeChar rel >>= pointContainsLineBreak
  editLineLiftGapBuffer (sliceFromCursor (unwrapRelative rel) >>= safeFreeze) >>=
    makeLineWithSlice (if break then id else const NoLineBreak)

-- | Copy the current line 'Before' or 'After' the cursor into a 'TextLine'. The content of the line
-- editor is not changed.
copyCharsToEnd
  :: (PrimMonad m, st ~ PrimState m)
  => RelativeToCursor -> EditLine tags m (TextLine tags)
copyCharsToEnd rel = editLineLiftGapBuffer (getSlice rel >>= safeFreeze) >>= case rel of
  Before -> makeLineWithSlice (const NoLineBreak)
  After  -> makeLineWithSlice id

-- | Like 'textLineIndex', but throws a 'LineIndexOutOfRange' exception if the given 'LineIndex' is
-- out of bounds.
validateCharIndex
  :: (PrimMonad m, st ~ PrimState m)
  => Absolute CharIndex -> EditLine tags m (Absolute CharIndex)
validateCharIndex = editLineLiftGapBuffer . (validateIndex >=> indexToAbsolute)

-- | Create a 'TextLine' by copying the the characters in the given range from the line under the
-- cursor.
copyCharsRange
  :: PrimMonad m
  => Absolute CharIndex -> Absolute CharIndex
  -> EditLine tags m (TextLine tags)
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
  :: PrimMonad m
  => Absolute CharIndex -> Absolute CharIndex
  -> EditLine tags m (TextLine tags)
copyCharsBetween from to = copyCharsRange (min from to) (max from to)

-- | Write a 'TextLine' (as produced by 'copyLineEditorText' or getLineAt') to an @('Absolute'
-- 'LineIndex')@ address.
putLineIndex
  :: (PrimMonad m, st ~ PrimState m)
  => Absolute LineIndex -> TextLine tags -> EditText tags m ()
putLineIndex i line = editTextLiftGapBuffer $
  validateIndex i >>= absoluteIndex >>= flip putElemIndex line

-- | Replace the content in the 'bufferLineEditor' with the content in the given 'TextLine'. This
-- function does not re-allocate the current line editor buffer unless it is too small to hold all
-- of the characters in the given 'TextLine', meaning this function only grows the buffer memory
-- allocation, it never shrinks the memory allocation.
refillLineEditor :: PrimMonad m => EditText tags m ()
refillLineEditor =
  let getCount = flip modCount id in
  ( 
    editTextLiftGapBuffer (getCount Before) >>= \ count ->
    if count >= 0 then editTextLiftGapBuffer $ popElem Before else
    editTextLiftGapBuffer (getCount After) >>= \ count ->
    if count >= 0 then editTextLiftGapBuffer $ popElem After else
    emptyTextLine <$>
    editLine (editLineState $ use lineEditorTags)
  ) >>=
  refillLineEditorWith

-- | Like 'refillLineEditor', but replaces the content in the 'bufferLineEditor' with the content in
-- a given 'TextLine', rather the content of the current line.
refillLineEditorWith :: PrimMonad m => TextLine tags -> EditText tags m ()
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
clearWholeLine :: (PrimMonad m, st ~ PrimState m) => EditLine tags m ()
clearWholeLine = do
  sym <- editLineState $ use lineBreakSymbol
  editLineLiftGapBuffer $ do
    modCount Before $ const 0
    modCount After  $ const $ case sym of { NoLineBreak -> 0; _ -> 1; }
    return ()

liftEditText :: Monad m => EditText tags m a -> EditLine tags m a
liftEditText = EditLine . lift . lift

-- | Like 'clearWholeLine', this function deletes the content of the 'bufferLineEditor', and tags
-- on this line are not effected. The difference is that this function replaces the
-- 'bufferLineEditor' with a new empty line, resetting the line editor buffer to the default
-- allocation size and allowing the garbage collector to delete the previous allocation. This means
-- the line editor buffer memory allocation may be shrunk to it's minimal/default size.
resetLineEditor :: PrimMonad m => EditText tags m ()
resetLineEditor = editTextState (use $ bufferLineEditor . lineEditorTags) >>=
  lift . newLineEditor Nothing >>= editTextState . assign bufferLineEditor

-- | This function copies the current 'LineEditor' state back to the 'TextBuffer', and sets a flag
-- indicating that the content of the 'LineEditor' and the content of the current line are identical
-- so as to prevent further copying. It also returns a copy of the 'TextLine' that was pushed to the
-- 'TextBuffer'. If the 'lineEditorIsClean' flag is 'True', this function does nothing. Note that
-- this function is called automatically by 'flushRefill'. Ordinarily, you would call 'flushRefill',
-- especially when calling the 'moveByLine' or 'gotoLine' functions, rather of using this function
-- directly.
--
-- In situations where you would /not/ evaluate 'moveByLine' or 'gotoLine' with the 'flushRefill'
-- function, you can choose when to copy the content of the 'LineEditor' back to the buffer after
-- moving it. This can be done to "move" the content of a line out of the 'TextBuffer', into the
-- 'LineEditor', and then move the content back into the 'TextBuffer' at a different location after
-- changing location. This can also be useful after accumulating the content of several lines of
-- text into the 'LineEditor'.
flushLineEditor :: PrimMonad m => EditText tags m (TextLine tags)
flushLineEditor = do
  clean <- editTextState $ use $ bufferLineEditor . lineEditorIsClean
  if clean then editTextLiftGapBuffer $ getElem Before else do
    let loop = -- pop all lines after this one that don't have a line breaking symbol
          countLines After >>= \ count ->
          -- check if we are at the end of the buffer
          if count <= 0 then editLine copyLineEditorText else do
            line <- popLine After
            let lbrk = line ^. textLineBreakSymbol
            editLine $ do
              moveByChar maxBound -- cursor over to end of buffer
              editLineLiftGapBuffer $ pushElemVec Before $ line ^. textLineString
              editLineState $ lineBreakSymbol .= lbrk
            if lbrk == NoLineBreak then loop else editLine copyLineEditorText
    lbrk <- editLine $ editLineState $ use lineBreakSymbol
     -- check the current line editor for a line break
    line <- if lbrk == NoLineBreak then loop else editLine copyLineEditorText
    editTextLiftGapBuffer $ putElem Before line
    editLine $ editLineLiftGapBuffer $ do
      count <- modCount After id
      when (count <= 0) $ shiftCursor (-1)
    editTextState $ bufferLineEditor . lineEditorIsClean .= True
    return line

-- | Evaluate a function on the 'TextLine' currently under the 'getLineNumber'. This function
-- throws an exception if the 'TextBuffer' is empty. __NOTE__ that the 'TextBuffer' is considered
-- empty if there are characters in the 'LineBuffer' which have not been flushed to the empty
-- 'TextBuffer' by either 'flushLineEditor' or 'flushRefill'.
withCurrentLine
  :: PrimMonad m
  => (Absolute LineIndex -> TextLine tags -> EditLine tags m a)
  -> EditText tags m a
withCurrentLine f = do
  (ln, line) <- (,) <$> getLineNumber <*> editTextLiftGapBuffer (getElem Before)
  case line of
    TextLineUndefined -> error $
      "internal error: "++show ln++" received from 'getLineAt' points to undefined line"
    line              -> editLine $ f ln line

-- | Return the number of lines of text in this buffer.
bufferLineCount :: (PrimMonad m, st ~ PrimState m) => EditText tags m VecLength
bufferLineCount = editTextLiftGapBuffer countDefined

-- | Returns a boolean indicating whether there is no content in the current buffer.
bufferIsEmpty :: PrimMonad m => EditText tags m Bool
bufferIsEmpty = bufferLineCount >>= \ ln ->
  if ln > 1 then return False
  else if ln <= 0 then return True
  else withCurrentLine $ \ _ line -> return $ textLineCursorSpan line == 0

-- | Return a pointer to the buffer currently being edited by the @editor@ function.
currentBuffer
  :: (PrimMonad m, st ~ PrimState m)
  => EditText tags m (TextBuffer m tags)
currentBuffer = editTextState $ use $ bufferLineEditor . parentTextEditor

----------------------------------------------------------------------------------------------------

---- not for export
--modifyColumn
--  :: (PrimMonad m, st ~ PrimState m)
--  => (Absolute CharIndex -> Absolute CharIndex -> Relative CharIndex)
--  -> EditLine tags m (Absolute CharIndex)
--modifyColumn f = do
--  lbrksym <- editLineState $ use lineBreakSymbol
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
flushRefill :: PrimMonad m => EditText tags m a -> EditText tags m a
flushRefill motion =
  editLine (editLineState $ use lineEditorIsClean) >>= \ clean ->
  if clean then motion else
  flushLineEditor >>
  motion <*
  refillLineEditor

----------------------------------------------------------------------------------------------------

-- | Insert a single character. If the 'lineBreakPredicate' function evaluates to 'Prelude.True',
-- meaning the given character is a line breaking character, this function does nothing. To insert
-- line breaks, using 'insertString'. Returns the number of characters added to the buffer.
--
-- __NOTE__ that this function does not keep track of whether you are inserting line breaks with
-- multiple characters such as the very common @"\CR\LF"@ character sequence. If you call
-- 'insertChar' with a @'\CR'@ and then a @'\LF'@ (for example: @'sequence' 'insertChar' "\CR\LF"@)
-- this will insert a line break ending with @'\CR'@, then it will insert an empty line with @'\LF'@
-- as a line break. Use the 'insertString' function to properly handle line breaks with multiple
-- characters.
insertChar
  :: (PrimMonad m, st ~ PrimState m)
  => RelativeToCursor -> Char
  -> EditText tags m CharCount
insertChar rel c = CharCount <$> do
  isBreak <- editTextState $ use $ bufferLineBreaker . lineBreakPredicate
  if isBreak c then return 0 else do
    editLine $ editLineLiftGapBuffer $ pushElem rel c
    editTextState $ bufferLineEditor . lineEditorIsClean .= False
    return 1

-- | This function evaluates the 'lineBreaker' function on the given string, and beginning from the
-- current cursor location, begins inserting all the lines of text produced by the 'lineBreaker'
-- function.
insertString
  :: (PrimMonad m, st ~ PrimState m)
  => String -> EditText tags m CharStats
insertString str = do
  breaker <- editTextState $ use $ bufferLineBreaker . lineBreaker
  let writeStr = editLine . editLineLiftGapBuffer .
        fmap sum . mapM ((>> (return 1)) . pushElem Before)
  let writeLine (str, lbrk) = do
        strlen <- writeStr (str ++ "\n")
        line   <- editLine $ do
          editLineState $ lineBreakSymbol .= lbrk
          copyLineEditorText <* editLineState (charsBeforeCursor .= 0 >> charsAfterCursor .= 0)
        when (lbrk /= NoLineBreak) $ editTextLiftGapBuffer $ pushElem Before line
        editTextState $ bufferLineEditor . lineEditorIsClean .= (lbrk /= NoLineBreak)
        return CharStats
          { cursorStepCount =
            toLength (unwrapCharCount strlen) +
            if lbrk == NoLineBreak then 0 else 1
          , deltaCharCount = strlen + lineBreakSize lbrk
          }
  let loop count = seq count . \ case
        []        -> return count
        line:more -> writeLine line >>= flip loop more . mappend count
  loop mempty $ breaker str

-- | Same as 'lineBreakWith', but uses the 'defaultLineBreak' value to break the line at the current
-- cursor location.
lineBreak
  :: (PrimMonad m, st ~ PrimState m)
  => RelativeToCursor -> EditText tags m ()
lineBreak rel =
  editTextState (use $ bufferLineBreaker . defaultLineBreakSymbol) >>= flip lineBreakWith rel

-- | Breaks the current line editor either 'Before' or 'After' the cursor using the given
-- 'LineBreakSymbol', creating a new 'TextLine' and pushing it to the 'TextBuffer' either 'Before'
-- or 'After' the cursor.
lineBreakWith
  :: (PrimMonad m, st ~ PrimState m)
  => LineBreakSymbol -> RelativeToCursor
  -> EditText tags m ()
lineBreakWith newlbrk rel = join $ editLine $ do
  tags    <- editLineState $ use lineEditorTags
  oldlbrk <- editLineState $ use lineBreakSymbol
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

-- | Delete the given number of characters within the line without traveling past the end of the
-- line. No error is thrown if the given number of characters is larger than the number of
-- characters available. The sign of the @'Relative' 'CharIndex'@ argument will determine the
-- direction of travel for the deletion -- negative will delete moving toward the beginning of the
-- line, positive will delete moving toward the end of the line. This function only deletes
-- characters on the current line, if the cursor is at the start of the line and you pass
-- @'minBound'@ as the second argument, this function does nothing.
--
-- The 'Bool' argument instructs 'deleteChars' whether or not to delete the 'lineBreakSymbol' from
-- this line as well if the @'Relative'@ number of characters requested is a positive 'CharIndex'
-- value larger than the number of characters available in the current 'LineEditor'.  Passing
-- 'False' as the 'Bool' argument will never delete the 'LineBreakSymbol' for this line regardless
-- of the @'Relative' 'CharIndex'@ value given as the second argument. Passing 'True' will delete
-- the 'lineBreakSymbol'.
--
-- __NOTE__ that passing 'True' and remvoing the line breaking symbol from the line editor will
-- change the behavior that happens when 'flushLineEditor' is used. Unless (before calling
-- 'flushLineEditor') a new line breaking symbol other than 'NoLineBreak' is set using
-- 'setLineBreakSymbol', flushing the characters will evaluate @'popLine' 'After'@ and append the
-- next line to the current line editor before flushing to ensure that a 'TextLine' with a proper
-- line break symbol is being pushed into the buffer. The only time this does not happen is if the
-- cursor is at the end of the 'TextBuffer', where it is OK to have exactly one 'TextLine' that does
-- not have a line break symbol.
deleteChars
  :: (PrimMonad m, st ~ PrimState m)
  => Bool -> Relative CharIndex -> EditLine tags m CharStats
deleteChars delBreak req@(Relative (CharIndex curspan0)) = do
  let curspan = VecLength $ abs curspan0
  lbrk <- editLineState $ use lineBreakSymbol
  let noChange = pure (0, 0, False, lbrk)
  (VecLength steps, VecLength chars, changed, lbrk) <- editLineLiftGapBuffer $
    if req < 0 then -- deleting before the cursor
      modCount Before id >>= \ count ->
      if count <= 0 then noChange else do
        let actual = negate $ min curspan count
        modCount Before (+ actual)
        return (actual, actual, True, lbrk)
    else if req > 0 then -- deleting after the cursror
      modCount After id >>= \ count ->
      if count <= 0 then noChange else
      if curspan < count || lbrk == NoLineBreak then do -- no need to worry about line break
        after <- modCount After $ subtract $ min curspan count
        let actual = after - count 
        return (actual, actual, True, lbrk)
      else do -- do need to worry about line break
        after <- modCount After $ const $ if delBreak then 0 else 1
        let actual = after - count
        return $ if delBreak then (actual, actual, True, lbrk) else 
          (actual, actual - lineBreakSize lbrk, True, NoLineBreak)
    else noChange
  editLineState $ do
    when changed $ lineEditorIsClean .= False
    lineBreakSymbol .= lbrk
  return CharStats
    { cursorStepCount = Relative $ CharIndex steps
    , deltaCharCount  = CharCount chars
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
  :: forall m tags
   . ( PrimMonad m
     , Show tags --DEBUG
     )
  => Relative CharIndex -> EditText tags m CharStats
  -- TODO: rewrite this to make use of the new pushLine/popLine behavior, it can be made much, much
  -- more elegant.
deleteCharsWrap request0 =
  if request0 == 0 then return mempty else
  let request = abs request0 in
  let (direction, sign) = if request < 0 then (Before, negate) else (After, id) in
  flushRefill (pure ()) >>
  editLine (deleteChars True request) >>= \ st ->
  if abs (cursorStepCount st) >= request then return st else
  rewriteLines direction (pure st, st)
  (\ halt line ->
    gets snd >>= \ st0 ->
    if abs (cursorStepCount st0) >= request then halt else
    let st = st0 <> opposite (textLineStats line) in
    let count = abs $ cursorStepCount st in
    if count > request then
      put (mappend st0 <$> editLine (deleteChars True $ sign $ request - count), st0) >>
      halt
    else
      put (pure st, st)
  ) >>=
  fst

---- TODO: uncomment this, write functions that actually make use of it.
----
---- Not for export, because this function does not return a proper accounting of the characters it
---- deletes, it is expected that 'CharStats' accounting will be done by the functions which call this
---- one.
----
---- This function overwites all text before or after the cursor with a 'TextLine'. If 'Before', the
---- line breaking characters from the given 'TextLine' are ignored, if 'After', the line breaking
---- characters for the current line are overwritten with the line breaking characters of the given
---- 'TextLine'. Pass a function of two arguments which combines the tags from (1) the current line
---- editor and (2) the given 'TextLine'.
--overwriteAtCursor
--  :: forall m st tags . (MonadIO m, PrimMonad m, st ~ PrimState m)
--  => RelativeToCursor
--  -> (tags -> tags -> tags)
--  -> TextLine tags
--  -> EditLine tags m ()
--overwriteAtCursor rel concatTags line = case line of
--  TextLineUndefined ->
--    error $ "overwriteAtCursor "++show rel++": received undefined TextLine"
--  TextLine
--   { theTextLineString      = line
--   , theTextLineBreakSymbol = lbrk
--   , theTextLineTags        = tagsB
--   } -> do
--    editLineLiftGapBuffer $ do
--      modCount rel $ const 0 -- this "deletes" the chars Before/After the cursor
--      count <- countDefined
--      alloc <- getAllocSize
--      let linelen  = VecLength $ UVec.length line
--      let adjusted = linelen - case rel of { Before -> lineBreakSize lbrk; After -> 0; }
--      let diffsize = adjusted + count - alloc
--      when (diffsize > 0) $ growVector diffsize -- resize line editor buffer
--      buf   <- use gapBufferVector
--      alloc <- getAllocSize -- new (resized) allocation value
--      lift $ case rel of
--        Before -> UVec.copy
--          (UMVec.slice 0 (fromLength adjusted) buf)
--          (UVec.slice 0 (fromLength adjusted) line)
--        After  -> UVec.copy
--          (UMVec.slice (fromLength $ alloc - adjusted) (fromLength adjusted) buf)
--          line
--    EditLine $ do
--      lineBreakSymbol .= lbrk
--      lineEditorIsClean .= False
--      lineEditorTags %= (`concatTags` tagsB)

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

instance Semigroup TextLocation where { (<>) = sumTextLocation; }

-- | Compute the difference between two 'TextLocations', works just like vector arithmetic.
diffTextLocation :: TextLocation -> TextLocation -> TextLocation
diffTextLocation a b = TextLocation
  { theLocationLineIndex = theLocationLineIndex a - theLocationLineIndex b
  , theLocationCharIndex = theLocationCharIndex a - theLocationCharIndex b
  }

-- | Vector sum of two 'TextLocation' values, works just like vector arithmetic.
sumTextLocation :: TextLocation -> TextLocation -> TextLocation
sumTextLocation a b = TextLocation
  { theLocationLineIndex = theLocationLineIndex a + theLocationLineIndex b
  , theLocationCharIndex = theLocationCharIndex a + theLocationCharIndex b
  }

-- | Construct a @'Relative' 'LineIndex'@ given a 'Before' of 'After' value and an integer number of
-- lines.
relativeLine :: RelativeToCursor -> Int -> Relative LineIndex
relativeChar :: RelativeToCursor -> Int -> Relative CharIndex
(relativeLine, relativeChar) = (f LineIndex, f CharIndex) where
  f constr = ((Relative . constr) .) . \ case { Before -> negate; After -> id; }

-- | Obtain the @'Absolute' 'LineIndex'@ from a 'TextLocation' value.
lineIndex :: Lens' TextLocation (Absolute LineIndex)
lineIndex = lens theLocationLineIndex $ \ a b -> a{ theLocationLineIndex = b }

-- | Obtain the @'Absolute' 'CharIndex'@ from a 'TextLocation' value.
charIndex :: Lens' TextLocation (Absolute CharIndex)
charIndex = lens theLocationCharIndex $ \ a b -> a{ theLocationCharIndex = b }

-- | A class of vales that can produce an 'Absolute' index from a 'Relative' index within the
-- context of an @editor@ with a cursor.
class RelativeToAbsoluteCursor index editor | editor -> index where
  -- | Convert a 'Relative' index (either a 'LineIndex' or 'CharIndex') to an 'Absolute' index.
  relativeToAbsolute :: Relative index -> editor (Absolute index)

instance
  (PrimMonad m, st ~ PrimState m
  ) => RelativeToAbsoluteCursor LineIndex (EditText tags m)
  where
    relativeToAbsolute = editTextLiftGapBuffer . (relativeIndex >=> indexToAbsolute)

instance
  (PrimMonad m, st ~ PrimState m
  ) => RelativeToAbsoluteCursor CharIndex (EditLine tags m)
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
  :: (PrimMonad m, st ~ PrimState m)
  => Maybe VecLength
    -- ^ Specify the additional amount of space to allocate for the buffer. This value will be added
    -- to the number of 'TextLine' elements in the 'TextFrame' to allocate the new bufer.
  -> TextLocation -- ^ The initial cursor position.
  -> tags            -- ^ Specify the default tags value.
  -> TextFrame tags  -- ^ The frame from which to fill the buffer.
  -> LineEditor m tags -- ^ An initial line editor, should be empty.
  -> m (TextBuffer m tags)
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
  :: (PrimMonad m, st ~ PrimState m)
  => TextLocation -> tags -> TextFrame tags -> m (TextBuffer m tags)
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
  :: (PrimMonad m, st ~ PrimState m)
  => TextLocation -> TextLocation -> EditText tags m (TextFrame tags)
textFrame from0 to0 = do
  (from, loline) <- validateLocation $ min from0 to0
  (to,   hiline) <- validateLocation $ max from0 to0
  if (from ^. lineIndex) == (to   ^. lineIndex) then
    (throwError ||| return) $ flip runViewLine loline $ do
      gotoChar (from ^. charIndex)
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
           flip runViewLine line $ gotoChar (point ^. charIndex) >> take <$> splitLine
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
  :: (PrimMonad m, st ~ PrimState m)
  => Absolute LineIndex -> Absolute LineIndex
  -> EditText tags m (TextFrame tags)
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

-- | Take the line under the cursor within a 'ViewText' function context, and use this line to
-- evaluate a 'ViewLine' function.
viewLine :: Monad m => ViewLine tags m a -> ViewText tags m a
viewLine f = getLineNumber >>= getLineAt >>= lift . runViewLineT f >>= (throwError ||| return)

----------------------------------------------------------------------------------------------------

type family EditorTagsType (editor :: * -> *)
type instance EditorTagsType (EditText tags m) = tags

class Monad editor => CursorIndexedText editor where
  -- | Get the line number of the cursror (number of lines from the start of the buffer).
  getLineNumber :: editor (Absolute LineIndex)

  -- | Get the 'TextLine' value at the given line number.
  getLineAt  :: EditorTagsType editor ~ tags => Absolute LineIndex -> editor (TextLine tags)

  -- | Get the line under the cursror or in front of the cursor.
  getLine :: EditorTagsType editor ~ tags => RelativeToCursor -> editor (TextLine tags)

  -- | Move the cursor to a different line by an number of lines relative to the cursor. A negative
  -- number indicates moving the cursor toward the start of the buffer, a positive number indicates
  -- moving the cursor toward the end of the buffer.
  --
  -- __NOTE__: When using this function as an action in a mutable 'EditText' function, __UNLIKE__
  -- the 'moveCursor' function, this function __DOES NOT__ evaluate 'flushLineEditor' or
  -- 'refillLineEditor'. This is to allow you to move the cursor while keeping the content of the
  -- line the cursor contains.
  moveByLine :: Relative LineIndex -> editor (Absolute LineIndex)

  -- | Go to an absolute line number, the first line is 1 (line 0 and below all send the cursor to
  -- line 1), the last line is 'Prelude.maxBound'. This function throws an exception if the given
  -- @'Absolute' 'LineIndex'@ is out of bounds. The 'goNearLine' function will move to the nearest
  -- in-bounds line without ever throwing an exception.
  --
  -- This function __DOES NOT__ evaluate 'flushLineEditor' or 'refillLineEditor', so if you evaluate
  -- this function before calling 'flushLineEditor', the changes made to the current line will not
  -- remain in place on the current line, rather they will be carried over to the new line to which
  -- 'gotoLine' has moved the cursor.
  gotoLine :: Absolute LineIndex -> editor (Absolute LineIndex)

  -- | Go to an absolute line number, the first line is 1 (line 0 and below all send the cursor to
  -- line 1), the last line is 'Prelude.maxBound'. Unlike 'gotoLine
  --
  -- This function __DOES NOT__ evaluate 'flushLineEditor' or 'refillLineEditor', so if you evaluate
  -- this function before calling 'flushLineEditor', the changes made to the current line will not
  -- remain in place on the current line, rather they will be carried over to the new line to which
  -- 'gotoLine' has moved the cursor.
  goNearLine :: Absolute LineIndex -> editor (Absolute LineIndex)

  -- | Returns the number of lines there are 'Before' or 'After' the cursor.
  countLines :: RelativeToCursor -> editor (Relative LineIndex)

instance PrimMonad m => CursorIndexedText (EditText tags m) where
  getLineNumber = editTextLiftGapBuffer $ writingIndex Before >>= indexToAbsolute
  getLineAt     = editTextLiftGapBuffer . (validateIndex >=> absoluteIndex >=> getElemIndex)
  getLine       = editTextLiftGapBuffer . getElem
  moveByLine    = editTextLiftGapBuffer . moveCursorBy
  gotoLine      = editTextLiftGapBuffer . moveCursorTo
  goNearLine    = editTextLiftGapBuffer . moveCursorNear
  countLines    = editTextLiftGapBuffer . fmap (toLength . fromLength) . flip modCount id

instance Monad m => CursorIndexedText (ViewText tags m) where
  getLineNumber = viewTextLiftIIBuffer $ writingIndex Before >>= indexToAbsolute
  getLineAt     = viewTextLiftIIBuffer . (validateIndex >=> absoluteIndex >=> getElemIndex)
  getLine       = viewTextLiftIIBuffer . getElem
  moveByLine    = viewTextLiftIIBuffer . moveCursorBy
  gotoLine      = viewTextLiftIIBuffer . moveCursorTo
  goNearLine    = viewTextLiftIIBuffer . moveCursorNear
  countLines    = viewTextLiftIIBuffer . fmap (toLength . fromLength) . flip modCount id

----------------------------------------------------------------------------------------------------

-- | This class provides functions that can be used by both 'EditLine' and 'ViewLine'.
class Monad editor => CursorIndexedLine editor where

  -- | Get the column number of the cursror (number of characters from the start of the current
  -- line).
  getColumnNumber   :: editor (Absolute CharIndex)

  -- | Get the line breaking symbol at the end of the current line.
  getLineBreakSymbol  :: editor LineBreakSymbol

  -- | For mutable buffers, this function gets the number of characters that the current line can
  -- store before re-allocating.
  getLineAllocation :: editor VecLength

  -- | Move the cursor to a different character location within the 'bufferLineEditor' by a number
  -- of characters relative to the cursor. A negative number indicates moving toward the start of
  -- the line, a positive number indicates moving toward the end of the line.
  --
  -- This function does not wrap the cursor if motion moves past the end or beginning of the line,
  -- to do this, evaluate 'moveByCharWrap'.
  --
  -- This function __DOES NOT__ evaluate 'flushLineEditor' or 'refillLineEditor'.
  moveByChar :: Relative CharIndex -> editor (Absolute CharIndex)

  -- | Go to an absolute character (column) number, the first character is 1 (character 0 and below
  -- all send the cursor to column 1), the last line is 'Prelude.maxBound'. This function throws an
  -- exception if the given @'Absolute' 'CharIndex'@ is out of bounds. The 'goNearChar' function
  -- will move to the nearest in-bounds line without ever throwing an exception.
  --
  -- This function __DOES NOT__ evaluate 'flushLineEditor' or 'refillLineEditor'.
  gotoChar :: Absolute CharIndex -> editor (Absolute CharIndex)

  -- | Go to an absolute character (column) number, the first character is 1 (character 0 and below
  -- all send the cursor to column 1), the last line is 'Prelude.maxBound'.
  --
  -- This function __DOES NOT__ evaluate 'flushLineEditor' or 'refillLineEditor'.
  goNearChar :: Absolute CharIndex -> editor (Absolute CharIndex)

  -- | Returns the number of chars there are 'Before' or 'After' the cursor.
  countChars :: RelativeToCursor -> editor (Relative CharIndex)

instance PrimMonad m => CursorIndexedLine (EditLine tags m) where
  getColumnNumber    = editLineLiftGapBuffer $ writingIndex Before >>= indexToAbsolute
  getLineBreakSymbol = editLineState (use lineBreakSymbol)
  getLineAllocation  = editLineLiftGapBuffer getAllocSize
  moveByChar         = editLineLiftGapBuffer . moveCursorBy
  gotoChar           = editLineLiftGapBuffer . moveCursorTo
  goNearChar         = editLineLiftGapBuffer . moveCursorNear
  countChars         = editLineLiftGapBuffer . fmap (toLength . fromLength) . flip modCount id

instance PrimMonad m => CursorIndexedLine (EditText tags m) where
  getColumnNumber    = editLine getColumnNumber
  getLineBreakSymbol = editLine getLineBreakSymbol
  getLineAllocation  = editLine getLineAllocation
  moveByChar         = editLine . moveByChar
  gotoChar           = editLine . gotoChar
  goNearChar         = editLine . goNearChar
  countChars         = editLine . countChars

instance Monad m => CursorIndexedLine (ViewLine tags m) where
  getColumnNumber    = viewLineLiftIIBuffer $ writingIndex Before >>= indexToAbsolute
  getLineBreakSymbol = theTextLineBreakSymbol <$> use viewerLine
  getLineAllocation  = viewLineLiftIIBuffer getAllocSize
  moveByChar         = viewLineLiftIIBuffer . moveCursorBy
  gotoChar           = viewLineLiftIIBuffer . moveCursorTo
  goNearChar         = viewLineLiftIIBuffer . moveCursorNear
  countChars         = viewLineLiftIIBuffer . fmap (toLength . fromLength) . flip modCount id

instance Monad m => CursorIndexedLine (ViewText tags m) where
  getColumnNumber    = viewLine getColumnNumber
  getLineBreakSymbol = viewLine getLineBreakSymbol
  getLineAllocation  = viewLine getLineAllocation 
  moveByChar         = viewLine . moveByChar
  gotoChar           = viewLine . gotoChar
  goNearChar         = viewLine . viewLineLiftIIBuffer . moveCursorNear
  countChars         = viewLine . countChars

getLineBreakSize :: CursorIndexedLine editor => editor VecLength
getLineBreakSize = lineBreakSize <$> getLineBreakSymbol

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

class (CursorIndexedLine editor, CursorIndexedText editor) => CursorIndexedBuffer editor where
  -- | This function calls 'moveByLine' and then 'moveByChar' to move the cursor by a number of
  -- lines and characters relative to the current cursor location.
  --
  -- __NOTE__: When using this function as an action in a mutable 'EditText' function, this will
  -- evaluate 'flushLineEditor' before moving the cursor, and evaluates 'refillLineEditor' after
  -- moving the cursor. This is necessary because it is impossible to instruct the 'LineEditor'
  -- which character location to move to unless it contains an accurate copy of the line of the
  -- buffer that it is currently editing.
  --
  -- If you want to move the cursor without flushing and refilling, use 'gotoLine' or
  -- 'moveByLine'. Then once you have decided what to do with the content of the current line
  -- editor, can move the cursor column with 'gotoChar' or 'moveByChar', or set the new target
  -- column with the expression @('bufferTargetCol' 'Control.Lens..=' c)@.
  moveCursor :: Relative LineIndex -> Relative CharIndex -> editor TextLocation

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
  gotoLocation :: TextLocation -> editor TextLocation

  -- | Like 'gotoLocation' but never throws an exception if the given 'TextLocation' is out of
  -- bounds.
  goNearLocation :: TextLocation -> editor TextLocation

instance PrimMonad m => CursorIndexedBuffer (EditText tags m) where
  moveCursor row col = TextLocation <$> flushRefill (moveByLine row) <*> moveByChar col
  gotoLocation (TextLocation{theLocationLineIndex=ln,theLocationCharIndex=ch}) =
    TextLocation <$> flushRefill (gotoLine ln) <*> gotoChar ch
  goNearLocation (TextLocation{theLocationLineIndex=ln,theLocationCharIndex=ch}) =
    TextLocation <$> flushRefill (goNearLine ln) <*> goNearChar ch

instance Monad m => CursorIndexedBuffer (ViewText tags m) where
  moveCursor row col = TextLocation <$> moveByLine row <*> moveByChar col
  gotoLocation (TextLocation{theLocationLineIndex=ln,theLocationCharIndex=ch}) =
    TextLocation <$> gotoLine ln <*> gotoChar ch
  goNearLocation (TextLocation{theLocationLineIndex=ln,theLocationCharIndex=ch}) =
    TextLocation <$> goNearLine ln <*> goNearChar ch

-- | Get the current cursor location. This function is identical to 'getLocation'.
getLocation :: (CursorIndexedLine editor, CursorIndexedText editor) => editor TextLocation
getLocation = TextLocation <$> getLineNumber <*> getColumnNumber

-- | Save the location of the cursor, then evaluate an @editor@ function. After evaluation
-- completes, restore the location of the cursor (within range, as the location may no longer exist)
-- and return the result of evaluation.
saveCursorEval :: CursorIndexedBuffer editor => editor a -> editor a
saveCursorEval f = do
  (cur, a) <- (,) <$> getLocation <*> f
  goNearLocation cur >> return a

-- | Compute the @'Relative' 'CharIndex'@ between two 'TextLocation's. The @'Relative' 'CharIndex'@
-- is the number of steps the text cursor must take to get from point @a@ to point @b@. For the most
-- part, this is equal to the number of characters that exist between point @a@ and point @b@,
-- except when line break characters consist of two characters (like the @'\\r\\n'@ combination)
-- and, only in this case, two characters are treated as one cursor step.
--
-- This function __DOES_NOT__ evlauate 'flushLineEditor', so you may get a results you are not
-- expecting if the text between the two given 'TextLocation's contains the line currently being
-- edited by the 'LineEditor'. If you want to make sure text in the 'LineEditor' is properly
-- accounted for in the result of this function, be sure to evaluate 'flushLineEditor' before
-- evaluating this function.
distanceBetween
  :: (PrimMonad m
     , Show tags --DEBUG
     )
  => TextLocation -> TextLocation -> EditText tags m (Relative CharIndex)
distanceBetween a0 b0 = do
  (a, lineA) <- validateLocation a0
  (b, lineB) <- validateLocation b0
  let forward = a <= b
  let ci = theLocationCharIndex
  let li = theLocationLineIndex
  let (liA, liB) = (li a, li b)
  let (sp, tcs) = (cursorStepCount . textLineStats, Relative . CharIndex)
  let edge line a b = sp line - tcs (fromIndex $ ci a) + tcs (fromIndex $ ci b)
  let edgeSize = if forward then edge lineA a b else edge lineB b a
  let (nextA, prevB) = if forward then (liA + 1, liB - 1) else (liA - 1, liB + 1)
  (if forward then id else negate) <$>
    if liA == liB then return $ tcs $ fromIndex (ci b) - fromIndex (ci a)
    else if nextA == liB || prevB == liA then return edgeSize
    else foldLinesBetween nextA prevB edgeSize (\ _halt _i -> modify . (+) . sp)

-- | Compute the 'TextLocation' where the cursor would end up if you were to count a given number of
-- cursor steps (a value given by @'Relative' 'CharIndex'@) from an initial 'TextLocation'. Also
-- returns the number of characters that were actually spanned, which may be less than the requested
-- number.
--
-- This function __DOES_NOT__ evlauate 'flushLineEditor', so you may get a results you are not
-- expecting if the text between the two given 'TextLocation's contains the line currently being
-- edited by the 'LineEditor'. If you want to make sure text in the 'LineEditor' is properly
-- accounted for in the result of this function, be sure to evaluate 'flushLineEditor' before
-- evaluating this function.
spanDistance
  :: (PrimMonad m
     , Show tags --DEBUG
     )
  => TextLocation -> Relative CharIndex -> EditText tags m (TextLocation, Relative CharIndex)
spanDistance a dist = do
  let absDist = abs dist
  let forward = dist >= 0
  let constr size ln ch = flip (,) size $ TextLocation
        { theLocationLineIndex = ln
        , theLocationCharIndex = ch
        }
  let foldTo = if forward then maxBound else minBound
  foldLinesInRange (theLocationLineIndex a) foldTo (a, 0) $ \ halt i line -> do
    let lineSize = cursorStepCount $ textLineStats line
    oldCount <- gets snd
    let newCount = oldCount + lineSize
    let update = put $ constr newCount (if forward then i + 1 else i) 1
    if newCount == absDist then update >> halt
    else if newCount > absDist then do
      put $ constr absDist i $ toIndex $ fromLength $
        if forward then absDist - oldCount else newCount - absDist
      halt
    else update

-- | Like 'moveByChar' but will wrap up to the previous line and continue moving on the
-- previous/next line if the value is large enough to move the cursor past the start\/end of the
-- line.
moveByCharWrap
  :: (PrimMonad m
     , Show tags --DEBUG
     )
  => Relative CharIndex -> EditText tags m TextLocation
moveByCharWrap dist = getLocation >>= flip spanDistance dist >>= gotoLocation . fst

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
  :: (PrimMonad m
     , Show tags
     ) => FoldMapChars a fold tags m a -> FoldMapLines r fold tags m a
foldMapChars f = get >>= foldMapLiftEditText . editLine . runFoldMapChars f >>= state . const

----------------------------------------------------------------------------------------------------

-- | A type synonym for a 'FoldMapLines' function in which the folded type is the unit @()@ value.
type MapChars r = FoldMapChars r ()

-- | Evaluate a 'MapChars' using 'evalFoldMapChars'.
runMapChars :: Monad m => MapChars a tags m a -> EditLine tags m a
runMapChars = flip evalFoldMapChars ()

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
  :: (PrimMonad m, st ~ PrimState m)
  => TextLocation -> TextLocation -> EditText tags m (StreamCursor tags)
newStreamCursorRange start end = streamResetCache StreamCursor
  { theStreamCache    = TextLineUndefined
  , theStreamLocation = min start end
  , theStreamEndpoint = max start end
  } >>= streamResetEndpoint (max start end)

-- | A convenience function that calls 'newStreamCursorRange' with 'minBound' as the 'TextLocation'.
newStreamCursor
  :: (PrimMonad m, st ~ PrimState m)
  => EditText tags m (StreamCursor tags)
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
  :: (PrimMonad m, st ~ PrimState m)
  => StreamCursor tags -> TextLocation
  -> EditText tags m (StreamCursor tags)
streamGoto cursor = validateLocation >=> \ (loc, txt) ->
  return cursor{ theStreamCache = txt, theStreamLocation = loc }

-- | Thinking of the 'StreamCursor' as a cursor pointing to a character within a 'TextBuffer' this
-- function returns the character currently under the cursor __without advancing the cursor.__ This
-- function throws an @'EndOfLineBuffer' 'After'@ exception if 'streamResetCache' cannot resolve
-- 'theStreamLocation'. This function throws an @'EndOfCharBuffer' 'After'@ exception if cursor has
-- somehow moved beyond 'theStreamCache', which could only happen if the 'streamResetCache' function
-- has not been called after transplanting the 'StreamCursor' to another 'TextBuffer'.
streamLook
  :: (PrimMonad m, st ~ PrimState m)
  => StreamCursor tags -> EditText tags m (Char, StreamCursor tags)
streamLook s = case theStreamCache s of
  TextLineUndefined -> streamResetCache s >>= streamLook
  line              -> case textLineGetChar line (theStreamLocation s ^. charIndex) of
    Nothing           -> throwError $ EndOfCharBuffer After
    Just  c           -> return (c, s)
{-# INLINE streamLook #-}

-- | Thinking of the 'StreamCursor' as a cursor pointing to a character within a 'TextBuffer' this
-- function advances the cursor without reading any characters.
streamStep
  :: (PrimMonad m, st ~ PrimState m)
  => StreamCursor tags -> EditText tags m (StreamCursor tags)
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
  :: (PrimMonad m, st ~ PrimState m)
  => StreamCursor tags -> EditText tags m (StreamCursor tags)
streamResetCache s = do
  (loc, txt) <- validateLocation (theStreamLocation s)
  return (s & streamLocation .~ loc & streamCache .~ txt)

-- | This function should only need to be evaluated once as long as the 'TextBuffer' never changes
-- suring the parsing process. If the 'TextBuffer' does change while the parsing has been paused,
-- this function must be evaluated to ensure the 'StreamCursor' is aware of the endpoint.
streamResetEndpoint
  :: (PrimMonad m, st ~ PrimState m)
  => TextLocation -> StreamCursor tags
  -> EditText tags m (StreamCursor tags)
streamResetEndpoint loc s = do
  (loc, _txt) <- validateLocation loc
  return (s & streamLocation .~ loc)

-- | You can use the 'streamTags' lens to alter the @tags@ value of the line cached (the line
-- returned by 'streamCache'd function), but to actually store these changes back to the
-- 'TextBuffer', this function must be called. This function is called automatically by the
-- 'streamStep' function when the cursor steps past the end of the current line and to the
-- next line.
streamCommitTags
  :: (PrimMonad m, st ~ PrimState m)
  => StreamCursor tags -> EditText tags m ()
streamCommitTags s = putLineIndex (s ^. streamLocation . lineIndex) (s ^. streamCache)

----------------------------------------------------------------------------------------------------

--ralign :: Int -> String
--ralign n = case abs n of
--  i | i < 10 -> "      " ++ show i
--  i | i < 100 -> "     " ++ show i
--  i | i < 1000 -> "    " ++ show i
--  i | i < 10000 -> "   " ++ show i
--  i | i < 100000 -> "  " ++ show i
--  i | i < 1000000 -> " " ++ show i
--  i                ->       show i

---- | Print debugger information about the structured data that forms the 'TextFrame' to standard
---- output. __WARNING:__ this print's every line of text in the view, so if your text view has
---- thousands of lines of text, there will be a lot of output.
--debugPrintView :: MonadIO m => (tags -> String) -> (String -> IO ()) -> TextFrame tags -> m ()
--debugPrintView showTags output view = do
--  (_, errCount) <- forLinesInView view (1 :: Int, 0 :: Int) $ \ _halt line -> do
--    lineNum <- state $ \ (lineNum, errCount) ->
--      (lineNum, (lineNum + 1, errCount + if textLineIsUndefined line then 1 else 0))
--    liftIO $ output $ ralign lineNum ++ ": " ++ showTextLine showTags line
--  liftIO $ output ""
--  when (errCount > 0) $ error $ "iterated over "++show errCount++" undefined lines"

---- | Print debugger information about the structured data that forms the 'TextBuffer' to standard
---- output. __WARNING:__ this print's every line of text in the buffer, so if your text buffer has
---- thousands of lines of text, there will be a lot of output.
--debugPrintBuffer
--  :: MonadIO m
--  => (tags -> String) -> (String -> IO ()) -> EditText tags m ()
--debugPrintBuffer showTags output = do
--  lineVec <- use bufferVector
--  let len = MVec.length lineVec
--  let printLines nullCount i = if i >= len then return () else do
--        line <- liftIO $
--          asrtMRead UnsafeOp "debugPrintBuffer" ("lineVec", lineVec) (asrtShow "i" i)
--        let showLine = output $ ralign i ++ ": " ++ showTextLine showTags line
--        if textLineIsUndefined line
--          then do
--            liftIO $ if nullCount < (1 :: Int) then showLine else
--              when (nullCount < 4) $ putStrLn "...."
--            (printLines $! nullCount + 1) $! i + 1
--          else liftIO showLine >> (printLines 0 $! i + 1)
--  printLines 0 0
--  above   <- use linesAboveCursor
--  below   <- use linesBelowCursor
--  liftIO $ do
--    output $ "   linesAboveCursor: " ++ show above
--    output $ "   linesBelowCursor: " ++ show below
--    output $ "    bufferLineCount: " ++ show (above + below)

---- | The 'bufferLineEditor', which is a 'LineEditor' is a separate data structure contained within
---- the 'TextBuffer', and it is often not necessary to know this information when debugging, so you
---- can print debugging information about the 'LineEditor' by evaluating this function whenever it is
---- necessary.
--debugPrintCursor
--  :: MonadIO m
--  => (tags -> String) -> (String -> IO ()) -> EditText tags m ()
--debugPrintCursor showTags output = do
--  cur <- use bufferLineEditor
--  let charVec    = theLineEditBuffer cur
--  let charVecLen = UMVec.length charVec
--  let before     = cur ^. charsBeforeCursor
--  let after      = cur ^. charsAfterCursor
--  liftIO $ do
--    str <- forM [0 .. charVecLen - 1] $
--      asrtMRead UnsafeOp "debugPrintCursor" ("charVec", charVec) . (,) "i"
--    output $ "     bufferLineEditor: " ++ show str
--    output $ "       lineEditorTags: " ++ showTags (cur ^. lineEditorTags)
--    output $ " __cursorVectorLength: " ++ show charVecLen
--    output $ "    charsBeforeCursor: " ++ show before
--    output $ "     charsAfterCursor: " ++ show after
--    output $ "      cursorInputSize: " ++ show (before + after)
--    output $ "  cursorLineBreakSize: " ++ show (theCursorBreakSize cur)
