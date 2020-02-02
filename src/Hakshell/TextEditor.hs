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
-- The "Hakshell.TextEditor" module is therefore designed to provide a solid, consistent foundation
-- for automated text processing APIs, upon which programmers can build text editors and text
-- processors, and hence a text editor API is an integral part of the "Hakshell" library.
module Hakshell.TextEditor
  ( -- * Text Buffers

    -- ** The 'TextLine' data type
    --
    -- A 'TextBuffer' is a memory-efficient vector containing 'TextLine's. A 'TextLine' is created
    -- by evaluating a 'TextEdit' function such as 'insertString'. A 'TextLine' within a
    -- 'TextBuffer' can be updated by navigating the cursor to the line using 'gotoChar' or
    -- 'moveByChar'. Moving the cursor to an address (addresses are line numbers) will copy the
    -- 'TextLine' at that address into a 'LineEditor'. You can then edit the text in a 'LineEditor'
    -- by evaluating a function of type 'EditLine' with the 'editLine' function.

    TextLine, emptyTextLine, nullTextLine, textLineCursorSpan, textLineGetChar,
    textLineTop, textLineIsUndefined, sliceLine, sliceLineToEnd, splitLine,

    -- *** 'TextLine' lenses

    textLineString, textLineTags, textLineChomp,

    -- ** The 'TextBuffer' data type
    --
    -- A 'TextBuffer' can be created with 'newTextBuffer', and then edited by evaluating a function
    -- of type 'EditText' on it with the 'runEditTextIO' function. You typically fill a 'TextBuffer'
    -- with a "String" using 'insertString'. A 'TextBuffer' contains a cursor which you can move
    -- around using the 'gotoChar', 'moveByChar', functions. Text is deleted with the
    -- 'deleteCharsWrap' functions.

    TextBuffer, newTextBuffer, copyTextBuffer,
    bufferLineBreaker, bufferDefaultTags,

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

    -- *** Creating line editors
    --
    -- Every 'TextBuffer' has a 'LineEditor' so there is usually no need to create your own.
    newLineEditor, copyLineEditor, --newLineEditorAt, --TODO

    -- ** The 'EditText' function type

    EditText, runEditTextIO, runEditTextOnCopy,

    insertString, lineBreak, deleteCharsWrap,
    pushLine, popLine, currentTextLocation, currentLineNumber, currentColumnNumber,
    getLineIndex, putLineIndex, withCurrentLine,
    bufferLineCount, bufferIsEmpty, currentBuffer,
    lineCursorIsDefined, 

    -- ** The 'EditLine' function type
    --
    -- Usually, functions of type 'EditLine' are evaluated by using the 'editLine' function within
    -- the context of an 'EditText' function. This places a line of text into a 'LineEditor' which
    -- allows the text to be edited character-by-character.

    EditLine, editLine, insertChar, deleteChars, lineEditorTags,
    copyCharsRange, copyCharsBetween, copyChars, copyCharsToEnd,
    charCursorIsDefined,
    -- TODO: getCharIndex, putCharIndex,

    -- ** Indicies and Bounds Checking

    CheckedIndex(..), validateLineIndex, lineIndexIsValid,
    validateGetLineIndex, testLocation, validateLocation, locationIsValid,
    validateCharIndex, testCharIndex, indexIsOnLineBreak,

    -- *** Error Data Type

    TextEditError(..),
    
    -- ** Moving the Cursor
    --
    -- __IMPORTANT__: when moving the cursor around using 'gotoPosition' or similar functions, the
    -- 'LineEditor' does not leave it's contents on the current line unless you explicitly call
    -- 'flushLineEditor'. Without "flushing" the characters currently in the 'LineEditor' will not
    -- be saved to the 'TextBuffer' and will be carried around to the 'LineEditor's current
    -- position. After moving to the new line, the 'LineEditor' will not load the content of the new
    -- line until you evaluate 'refillLineEditor'.
    --
    -- If you want the "normal" text editor behavior, where the 'LineEditor' leaves the characters
    -- currenly being edited on the current line, moves to a new line, and then begins editing the
    -- characters on that new line, you must evaluate a cursor motion value with 'flushRefill'.
    --
    -- There are situations where you may not want to use 'flushRefill' to perform a cursor motion, as
    -- in when accumulating lines into the 'LineEditor' after each cursor motion.

    getPosition, gotoPosition, saveCursorEval, gotoLine, gotoChar,

    Absolute(..),  LineIndex(..), CharIndex(..), TextLocation(..),
    TextCursorSpan(..), CharStats(..),
    shiftAbsolute, diffAbsolute, unwrapTextCursorSpan,
    lineToIndex, charToIndex, indexToLine, indexToChar,
    distanceBetween, spanDistance, sumTextLocation, diffTextLocation,

    -- *** Moving the cursor relative to the cursor

    moveCursor, moveByLine, moveByChar, moveByCharWrap,

    Relative(..), RelativeToCursor(..),
    RelativeToAbsoluteCursor, -- <- does not export members
    relativeToAbsolute, relativeLine, relativeChar, lineIndex, charIndex,
    lineToCount, charToCount, countToLine, countToChar,

    -- ** Text Views
    --
    -- A 'TextView' is an immutable copy of a portion of a 'TextBuffer'. By creating a 'TextView'
    -- you take a snapshot of a 'TextBuffer' at a point in time which can then be pasted into
    -- another 'TextBuffer', or used to draw the window of an interactive text editor application.

    TextView, textView, textViewOnLines, textViewAppend, emptyTextView,
    newTextBufferFromView, textViewCharCount, textViewVector,
    textViewToList, textViewToStrings,

    -- *** Fold over a 'TextView'.
    --
    -- It is possible to perform a folding operation on a 'TextView', this is often a good way to
    -- draw a graphical represntation of text, or to translate the 'TextView' to some other data
    -- type.

    FoldTextView, forLinesInView,

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

    -- * Parser Stream
    --
    -- To perform parsing of text, use the "Hakshell.TextEditor.Parser" module. The 'StreamCursor'
    -- provided here provides stateful information necessary to efficiently deliver a stream of
    -- characters from a 'TextBuffer' to a 'Hakshell.TextEditor.Parser.Parser'.

    StreamCursor, newStreamCursorRange, newStreamCursor,
    streamGoto, streamLook, streamStep, streamIsEOF, streamIsEOL,
    streamTags, streamCommitTags, streamResetCache, streamResetEndpoint,
    theStreamCache, theStreamLocation, theStreamEndpoint,

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

import           Hakshell.GapBuffer
import           Hakshell.String
import           Hakshell.TextEditor.LineBreaker

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

----------------------------------------------------------------------------------------------------

-- Used by 'newTextBuffer' as a parameter to 'newTextBufferState'.
defaultInitBufferSize :: Int
defaultInitBufferSize = 512

-- Overflow-safe absolute value function. Many programming language compilers have unintuitive
-- behavior when evaluating the expresion @(abs minBound)@ due to integer overflow, GHC also suffers
-- from this problem
safeAbs :: (Ord n, Num n, Bounded n) => n -> n
safeAbs n = if n >= 0 then n else negate $ if n == minBound then n + 1 else n

-- | Like the @[a .. b]@ notation when @(a <= b)@, but iterates in decreacing order when @(a > b)@.
iter2way :: (Ord n, Num n) => n -> n -> [n]
iter2way from to =
  takeWhile (flip (if from <= to then (<=) else (>=)) to) $
  iterate ((if from <= to then (+) else subtract) 1) from

----------------------------------------------------------------------------------------------------

data TextEditError
  = BufferIsEmpty
  | TextEditError       !StrictBytes
  | EndOfLineBuffer     !RelativeToCursor
  | EndOfCharBuffer     !RelativeToCursor
  | LineIndexOutOfRange !(Absolute LineIndex)
  | LineCountOutOfRange !(Relative LineIndex)
  | CharIndexOutOfRange !(Absolute CharIndex)
  | CharCountOutOfRange !(Relative CharIndex)
  deriving (Eq, Ord, Show)

----------------------------------------------------------------------------------------------------

-- | This function checks if a 'LineIndex' is valid. It first checks if the given index is an
-- extreme value, 'minBound' or 'maxBound', and returns an in-bounds index at the begninning or end
-- (respectively) of the valid range. If the value is in bounds or an extreme value, this function
-- returns 'True' along with a non-extreme, valid index value. If the index is out of bounds and not
-- an extreme value, this function returns 'False' along with an in-bound index value nearest to the
-- given index value.
--
-- If the 'TextBuffer' is completely empty, and 'flushLineEditor' has not been evaluated since the
-- 'TextBuffer' was first created or since it has been emptied, then this function throws a
-- 'BufferIsEmpty' exception.
--
-- This function can be used with either a @('Relative' 'LineIndex')@ or an
-- @('Absolute' 'LineIndex')@.
class (Monad m, MonadEditVec vec m, MonadError TextEditError m) => CheckedIndex i m vec where

  throwOutOfRangeIndex :: (MonadError TextEditError m) => i -> m void

  -- | This funtion tests whether an index @i@ is in range and returns the index paired with 'True'
  -- if it is. If the index @i@ is out of range, 'False' is returned paired with the nearest
  -- in-range index value.
  testIndex :: i -> m (Bool, i)

  validateIndex :: i -> m i
  validateIndex i0 = testIndex i0 >>= \ (ok, i) ->
    if ok then return i else throwOutOfRangeIndex i0

-- | This is the most general index testing function, it is used to instantiate 'testIndex' in the
-- 'CheckedIndex' class for both the 'EditText' and 'EditLine' monads. The difference between
-- 'textIndex' and 'testIndex' is that this 'testIndex' function takes a parameter of type
-- @''Absolute' 'Int')@ whereas 'testIndex' takes a type of @('Absolute' 'LineIndex')@ or
-- @('Absolute' 'CharIndex')@ depending on the monadic context in which it is called.
generalTestIndex
  :: (Eq i, IsIndex i, Bounded i, Num i, MonadEditVec vec m, MonadError TextEditError m)
  => i -> m (Bool, i)
generalTestIndex i = countDefined >>= \ (VecLength count) ->
  if count == 0 then throwError BufferIsEmpty else return $
  if      i == minBound        then (True , 0)
  else if i == maxBound        then (True , toIndex $ count - 1)
  else if fromIndex i >= count then (False, toIndex $ count - 1)
  else if fromIndex i <  0     then (False, 0)
  else                              (True , i)

-- | Like 'textLineIndex', but throws a 'LineIndexOutOfRange' exception if the given 'LineIndex' is
-- out of bounds.
validateLineIndex
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Absolute LineIndex -> EditText tags m (Absolute LineIndex)
validateLineIndex i0 = testIndex i0 >>= \ (ok, i) ->
  if ok then return i else throwOutOfRangeIndex i0

-- | Like 'validateLineIndex', but rather than throwing an exception, simply returns 'False' if an
-- exception would have been thrown, and returns 'True' otherwise.
lineIndexIsValid
  :: (CheckedIndex i (EditText tags m) (MVec.IOVector (TextLine tags))
     , Show tags --DEBUG
     )
  => i -> EditText tags m Bool
lineIndexIsValid = (`catchError` (const $ return False)) . fmap fst . testIndex

-- | Evaluates 'validateLineIndex' on a given 'LineIndex', and if no exceptions are thrown, also
-- dereferences the index, returning the 'TextLine' stored at the given valid index.
validateGetLineIndex
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Absolute LineIndex -> EditText tags m (Absolute LineIndex, TextLine tags)
validateGetLineIndex = validateLineIndex >=> \ i -> (,) i <$> (absoluteIndex i >>= getElemIndex)

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
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => TextLocation -> EditText tags m (Either TextLocation (TextLine tags, (Bool, TextLocation)))
testLocation loc = do
  (ok, linum) <- testIndex $ loc ^. lineIndex
  loc <- pure (loc & lineIndex .~ linum)
  if not ok then return $ Left loc else do
    textln <- absoluteIndex linum >>= getElemIndex
    (ok, i) <- liftViewLine (testIndex $ loc ^. charIndex) textln
    return $ Right (textln, (ok, loc & charIndex .~ i))

-- | Like 'testLocation', but evaluates to an exception if the 'TextLocation' is invalid.
validateLocation
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => TextLocation -> EditText tags m (TextLocation, TextLine tags)
validateLocation loc0 = testLocation loc0 >>= \ case
  Left{}  -> throwError $ LineIndexOutOfRange $ loc0 ^. lineIndex
  Right (textln, (ok, loc)) -> if ok then return (loc, textln) else
    throwError $ CharIndexOutOfRange $ loc0 ^. charIndex

-- | Like 'validateLocation' but rather than throwing an exception, returns 'Nothing'. If the
-- location is valid, the 'TextLine' at the valid 'TextLocation' is returned as a 'Just' value.
locationIsValid
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => TextLocation -> EditText tags m (Maybe (TextLine tags))
locationIsValid = (`catchError` (const $ return Nothing)) . fmap (Just . snd) . validateLocation

-- | This predicate function checks if an @('Absolute' 'CharIndex')@ points to the line breaking
-- character beyond the last character in a 'TextLine'. When single-stepping a 'TextLocation' cursor
-- such that you evaluate 'locationIsValid' after every step, it is important to evaluate this
-- function on the resultant 'TextLine', and if this function evaluates to 'True' you must increment
-- the 'lineIndex' and reset the 'charIndex' to @1@.
indexIsOnLineBreak :: TextLine tags -> VecIndex -> Bool
indexIsOnLineBreak line = ((unwrapTextCursorSpan $ textLineCursorSpan line) ==) . fromIndex

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

-- | When instructing the editor engine to move by or delete a number of cursor positions (where
-- line breaking characters consisting of two characters are considered a single cursor position,
-- thus the 'moveByCharWrap' or 'deleteCharsWrap' functions), you must specify the number of cursor
-- positions to move or delete using a value of this type.
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
-- When a user positions the cursor, it is done with 1-based indexing values of type 'LineIndex' or
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

-- | This data structure contains information about the number of character positions traversed
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

instance (Monad m, Semigroup a) => Semigroup (EditText tags m a) where
  a <> b = (<>) <$> a <*> b

instance (Monad m, Monoid a) => Monoid (EditText tags m a) where
  mempty = return mempty
  mappend a b = mappend <$> a <*> b

instance MonadIO m => MonadEditVec (MVec.IOVector (TextLine tags)) (EditText tags m) where
  getAllocSize = VecLength . GMVec.length <$> modVector id
  modVector f = state $ \ st -> let vec = f $ theBufferVector st in
    (vec, st{ theBufferVector = vec })
  modCount rel f = state $ \ st -> case rel of
    Before -> let i = f $ theLinesAboveCursor st in (i, st{ theLinesAboveCursor = i })
    After  -> let i = f $ theLinesBelowCursor st in (i, st{ theLinesBelowCursor = i })
  sliceVector (VecIndex i) (VecLength siz) = GMVec.slice i siz <$> modVector id
  getCursorIsDefined = use lineCursorIsDefined
  throwLimitErr = throwError . EndOfLineBuffer

instance MonadIO m => MonadGapBuffer (MVec.IOVector (TextLine tags)) (EditText tags m) where
  nullElem = pure TextLineUndefined
  newVector (VecLength siz) = nullElem >>= liftIO . MVec.replicate siz
  setCursorIsDefined = assign lineCursorIsDefined

instance
  (MonadIO m
  , Show tags --DEBUG
  ) =>
  CheckedIndex (Absolute LineIndex) (EditText tags m) (MVec.IOVector (TextLine tags))
  where
    throwOutOfRangeIndex = throwError . LineIndexOutOfRange
    testIndex = generalTestIndex

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

-- | This is a read-only folding function, unlikes 'FoldMapLines' or 'MapLines' which can perform
-- updates to the text in the buffer. 'FoldLines' functions will never change the position of the
-- text cursor, but you must be sure to call 'flushLineEditor' before evaluating 'foldLines' or
-- 'foldLinesInRange' if you want the latest updates to the 'LineEditor' to be included in the fold
-- result.
newtype FoldLines r fold tags m a
  = FoldLines
    { unwrapFoldLines :: ContT r (ExceptT TextEditError (StateT fold (EditText tags m))) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => MonadState fold (FoldLines r fold tags m) where
  state = FoldLines . lift . lift . state

instance Monad m => MonadError TextEditError (FoldLines r fold tags m) where
  throwError = FoldLines . lift . throwError
  catchError (FoldLines try) catch =
    FoldLines $ ContT $ \ next ->
    catchError (runContT try next) $ \ err ->
    runContT (unwrapFoldLines $ catch err) next

instance Monad m => MonadCont (FoldLines r fold tags m) where
  callCC f = FoldLines $ callCC $ unwrapFoldLines . f . fmap FoldLines

instance MonadTrans (FoldLines r fold tags) where
  lift = FoldLines . lift . lift . lift . lift

instance Semigroup a => Semigroup (FoldLines r fold tags m a) where
  a <> b = (<>) <$> a <*> b

instance Monoid a => Monoid (FoldLines r fold tags m a) where
  mappend a b = mappend <$> a <*> b
  mempty = return mempty

instance MonadIO m => MonadEditVec (MVec.IOVector (TextLine tags)) (FoldLines r fold tags m) where
  modVector          = foldLiftEditText . modVector
  modCount dir       = foldLiftEditText . modCount dir
  throwLimitErr      = foldLiftEditText . throwLimitErr

foldLiftEditText :: Monad m => EditText tags m a -> FoldLines r fold tags m a
foldLiftEditText = FoldLines . lift . lift . lift

-- | Evaluate a 'FoldLines' function to a 'EditText' function without performing any looping.
runFoldLinesStep :: Monad m => FoldLines a fold tags m a -> fold -> EditText tags m (a, fold)
runFoldLinesStep (FoldLines f) = runStateT (runExceptT $ runContT f return) >=> \ case
  (Left err, _   ) -> throwError err
  (Right  a, fold) -> return (a, fold)

-- | Evaluate a 'FoldLines' function between two @('Absolute' 'LineIndex')@ markers, including the
-- given line indicies.
foldLinesInRange
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Absolute LineIndex
  -> Absolute LineIndex
  -> fold
  -> ((() -> FoldLines () fold tags m void)
      -> Absolute LineIndex -> TextLine tags -> FoldLines () fold tags m ())
  -> EditText tags m fold
foldLinesInRange from to st fold = do
  top  <- indexNearCursor Before
  from <- validateIndex from
  to   <- validateIndex to
  let top slice = toIndex $ GMVec.length slice - 1
  let mid neg start = (start +) . neg . top
  let runFold = fmap snd . flip runFoldLinesStep st . callCC
  let runLoop (from, to) slice halt =
        mapM_ (\ (ln, i) -> getElemIndex i >>= fold halt ln) $
        zip (iter2way from to) $ (if from <= to then id else flip) iter2way 0 $ top slice
  let contained _ = runFold . runLoop (from, to)
  let straddle a b = runFold $ \ halt -> do
        let (rngA, rngB) = if from <= to
              then let m = mid  id  from a in ((from, m), (m + 1, to))
              else let m = mid negate to b in ((to, m), (m - 1, from))
        runLoop rngA a halt
        runLoop rngB b halt
  join $ withRegion contained straddle <$> absoluteIndex from <*> absoluteIndex to

foldLines
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Relative LineIndex
  -> fold
  -> ((() -> FoldLines () fold tags m void)
      -> Absolute LineIndex -> TextLine tags -> FoldLines () fold tags m ())
  -> EditText tags m fold
foldLines rel fold f = do
  from <- currentLineNumber
  let to = shiftAbsolute from rel
  foldLinesInRange from to fold f

foldLinesInBuffer
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => fold
  -> ((() -> FoldLines () fold tags m void)
      -> Absolute LineIndex -> TextLine tags -> FoldLines () fold tags m ())
  -> EditText tags m fold
foldLinesInBuffer = foldLinesInRange minBound maxBound

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

-- | Functions of this type operate on a single 'TextLine', it is useful for updating the
-- 'bufferLineEditor'.
newtype EditLine tags m a
  = EditLine
    { unwrapEditLine :: ExceptT TextEditError (StateT (LineEditor tags) (EditText tags m)) a }
  deriving (Functor, Applicative, Monad, MonadIO)
  -- TODO: try lifting @m@ instead of @(EditText tags m)@

instance Monad m => MonadState (LineEditor tags) (EditLine tags m) where
  state = EditLine . lift . state

instance Monad m =>  MonadError TextEditError (EditLine tags m) where
  throwError = EditLine . throwError
  catchError (EditLine try) catch = EditLine $ catchError try $ unwrapEditLine . catch

instance MonadTrans (EditLine tags) where
  lift = EditLine . lift . lift . lift

instance MonadIO m => MonadEditVec (UMVec.IOVector Char) (EditLine tags m) where
  getAllocSize = VecLength . GMVec.length <$> modVector id
  modVector f = modifying lineEditBuffer f >> use lineEditBuffer
  sliceVector (VecIndex i) (VecLength siz) = GMVec.slice i siz <$> modVector id
  modCount rel f = case rel of
    Before -> modifying charsBeforeCursor f >> use charsBeforeCursor
    After  -> modifying charsAfterCursor  f >> use charsAfterCursor
  getCursorIsDefined = use charCursorIsDefined
  throwLimitErr = throwError . EndOfCharBuffer

instance MonadIO m => MonadGapBuffer (UMVec.IOVector Char) (EditLine tags m) where
  nullElem = pure '\0'
  newVector (VecLength siz) = nullElem >>= liftIO . UMVec.replicate siz
  setCursorIsDefined = assign charCursorIsDefined

instance
  (MonadIO m
  , Show tags --DEBUG
  ) =>
  CheckedIndex (Absolute CharIndex) (EditLine tags m) (UMVec.IOVector Char)
  where
    throwOutOfRangeIndex = throwError . CharIndexOutOfRange
    testIndex = generalTestIndex

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
    { theBufferDefaultLine   :: !(TextLine tags)
      -- ^ The tag value to use when new 'TextLine's are automatically constructed after a line
      -- break character is inserted.
    , theBufferLineBreaker   :: LineBreaker
      -- ^ The function used to break strings into lines. This function is called every time a
      -- string is transferred from 'theBufferLineEditor' to to 'theLinesAbove' or 'theLinesBelow'.
    , theBufferVector        :: !(MVec.IOVector (TextLine tags))
      -- ^ A mutable vector containing each line of editable text.
    , theLinesAboveCursor    :: !VecLength
      -- ^ The number of lines above the cursor
    , theLinesBelowCursor    :: !VecLength
      -- ^ The number of line below the cursor
    , theLineCursorIsDefined :: !Bool
      -- ^ Indicates whether the item under the cursor is defined or undefined.
    , theBufferLineEditor    :: !(LineEditor tags)
      -- ^ A data structure for editing individual characters in a line of text.
    , theBufferTargetCol     :: !(Absolute CharIndex)
      -- ^ When moving the cursor up and down the 'TextBuffer' (e.g. using 'gotoPosition'), the
      -- cursor should generally remain at the same character column position.
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
    , theTextLineBreakSize :: !Word16
    , theTextLineTags      :: !tags
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

-- | The null-terminated, UTF-8 encoded string of bytes stored in this line of text.
textLineString :: Lens' (TextLine tags) CharVector
textLineString = lens theTextLineString $ \ a b -> a{ theTextLineString = b }

-- | Arbitrary information stored with this text. Typcial use cases for this field may include
-- syntax coloring rules, structured data parsed from the 'textLineString', text search indicies, a
-- diff of changes made, or all of the above.
textLineTags :: Lens' (TextLine tags) tags
textLineTags = lens theTextLineTags $ \ a b -> a{ theTextLineTags = b }

-- not for export
textLineBreakSize :: Lens' (TextLine tags) Word16
textLineBreakSize = lens theTextLineBreakSize $ \ a b -> a{ theTextLineBreakSize = b }

-- | Like 'unpack', but removes the terminating line-breaking characters.
textLineChomp :: TextLine tags -> String
textLineChomp = \ case
  TextLineUndefined -> ""
  TextLine{theTextLineString=vec,theTextLineBreakSize=lbrksz} ->
    UVec.toList $ UVec.slice 0 (UVec.length vec - fromIntegral lbrksz) vec

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
textLineCursorSpan line = TextCursorSpan $ intSize line - breaksize + min 1 (max 0 breaksize) where
  breaksize = fromIntegral $ theTextLineBreakSize line

-- | Return index of the top-most non-line-breaking character. The size of the string not including
-- the line breaking characters is equal to the result of this function evaluated by 'charToIndex'.
textLineTop :: TextLine tags -> Absolute CharIndex
textLineTop line = toIndex $ intSize line - (fromIntegral $ theTextLineBreakSize line)

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
data LineViewer tags
  = LineViewer
    { theViewerElemsBeforeCursor :: VecLength
    , theViewerLine :: TextLine tags
    }

viewerElemsBeforeCursor :: Lens' (LineViewer tags) VecLength
viewerElemsBeforeCursor =
  lens theViewerElemsBeforeCursor $ \ a b -> a{ theViewerElemsBeforeCursor = b }

viewerMirrorCursor :: LineViewer tags -> Iso' VecLength VecLength
viewerMirrorCursor view = iso (len -) (len -) where
  str = view ^. viewerLine . textLineString
  len = VecLength $ intSize str

viewerElemsAfterCursor :: Lens' (LineViewer tags) VecLength
viewerElemsAfterCursor = lens
  (\ view -> view ^. viewerElemsBeforeCursor . viewerMirrorCursor view)
  (\ view i -> view & viewerElemsBeforeCursor . viewerMirrorCursor view .~ i)

viewerLine :: Lens' (LineViewer tags) (TextLine tags)
viewerLine = lens theViewerLine $ \ a b -> a{ theViewerLine = b }

----------------------------------------------------------------------------------------------------

newtype ViewLine tags a
  = ViewLine
    { unwrapViewLine :: ExceptT TextEditError (State (LineViewer tags)) a }
  deriving (Functor, Applicative, Monad)

instance MonadState (LineViewer tags) (ViewLine tags) where
  state = ViewLine . state

instance MonadError TextEditError (ViewLine tags) where
  throwError = ViewLine . throwError
  catchError (ViewLine try) catch = ViewLine $ catchError try $ unwrapViewLine . catch

instance MonadEditVec CharVector (ViewLine tags) where
  getAllocSize = VecLength . intSize <$> use (viewerLine . textLineString)
  modVector f = modifying (viewerLine . textLineString) f >> use (viewerLine . textLineString)
  modCount rel f = case rel of
    Before -> modifying viewerElemsBeforeCursor f >> use viewerElemsBeforeCursor
    After  -> modifying viewerElemsAfterCursor  f >> use viewerElemsAfterCursor
  sliceVector (VecIndex i) (VecLength siz) =
    GVec.slice i siz <$> use (viewerLine . textLineString)
  throwLimitErr = throwError . EndOfCharBuffer

instance CheckedIndex (Absolute CharIndex) (ViewLine tags) CharVector where
  throwOutOfRangeIndex = throwError . CharIndexOutOfRange
  testIndex = generalTestIndex

liftViewLine :: MonadError TextEditError m => ViewLine tags a -> TextLine tags -> m a
liftViewLine f = (throwError ||| pure) . runViewLine f

runViewLine :: ViewLine tags a -> TextLine tags -> Either TextEditError a
runViewLine (ViewLine f) = \ case
  TextLineUndefined -> error "internal error: evaluated 'runViewLine' on an undefined line"
  line -> evalState (runExceptT f) $
    LineViewer{ theViewerElemsBeforeCursor = 0, theViewerLine = line }

-- | The top-most non-line-breaking character.
viewerTopIndex :: ViewLine tag (Absolute CharIndex)
viewerTopIndex = textLineTop <$> use viewerLine

-- Not for export
viewerTopValidIndex :: ViewLine tag VecIndex
viewerTopValidIndex = indexAfterRange 0 <$> countDefined

-- | Return the 'VecIndex' if the current cursor position was positioned from the end of the
-- string. Said another way, if the 'TextLine' is reversed and you had to keep the current cursor
-- position on the same character, what index would the cursor position be in the reversed
-- 'TextLine'.
viewerMirrorIndex :: ViewLine tag VecIndex
viewerMirrorIndex = indexAtRangeEnd 0 <$>
  (subtract <$> use viewerElemsBeforeCursor <*> getAllocSize)

viewerSetVecIndex :: VecIndex -> ViewLine tags ()
viewerSetVecIndex (VecIndex i) = viewerElemsBeforeCursor .= VecLength i

cursorIsOnLineBreak :: ViewLine tags Bool
cursorIsOnLineBreak = indexIsOnLineBreak <$> use viewerLine <*> cursorIndex

-- | Move the cursor of the 'TextView' to the given 'Absolute' 'CharIndex'.
viewerCursorTo :: Absolute CharIndex -> ViewLine tags ()
viewerCursorTo = validateIndex >=> absoluteIndex >=> viewerSetVecIndex

-- Not for export
--
-- Copies the tags and updates the line break size
similarLine :: (Word16 -> Word16) -> CharVector -> ViewLine tags (TextLine tags)
similarLine onLbrk vec = do
  line <- use viewerLine
  pure $ (textLineBreakSize %~ onLbrk) . (textLineString .~ vec) $ line

-- | Cut and remove a line at some index, keeping the characters 'Before' or 'After' the index.
sliceLineToEnd
  ::
    (Show tags) => --DEBUG
    RelativeToCursor -> ViewLine tags (TextLine tags)
sliceLineToEnd rel = getSlice rel >>= case rel of
  Before -> similarLine (const 0)
  After  -> similarLine id

-- | Splits the current 'TextLine' at the current cursor position.
splitLine
  ::
    (Show tags) --DEBUG
  => ViewLine tags (TextLine tags, TextLine tags)
splitLine = (,) <$> sliceLineToEnd Before <*> sliceLineToEnd After

-- | Cut the string within a 'TextLine' into a smaller substring.
sliceLine
  ::
    (Show tags) => --DEBUG
    Absolute CharIndex -> Relative CharIndex
  -> ViewLine tags (TextLine tags)
sliceLine from siz = viewerCursorTo from >>
  if      siz == 0        then emptyTextLine <$> use (viewerLine . textLineTags)
  else if siz == maxBound then sliceLineToEnd After
  else if siz == minBound then sliceLineToEnd Before
  else do
    (from, to) <- (uncurry min &&& uncurry max) <$>
      ((,) <$> cursorIndex <*> relativeIndex siz)
    islbrk <- cursorIsOnLineBreak
    withRegion (const pure) ((.) pure . mappend) from to >>=
      similarLine (if islbrk then id else const 0)

----------------------------------------------------------------------------------------------------

-- | The current line that is being edited.
--
-- A line editor needs to have it's characters flushed to it's parent 'TextBuffer' by the
-- 'flushLineEditor' in order for the changes in the 'LineEditor' to be applied to the document in
-- the 'TextBuffer'. The content of a 'LineEditor' can be reverted to the content of the line
-- currently under the cursor by calling 'refillLineEditor'.
--
-- Some cursor motion functions, like 'moveCursor' and 'gotoPosition' automatically evaluate
-- 'flushLineEditor' before moving the cursor and automatically evaluate 'refillLineEditor' after
-- moving the cursor. Other cursor motion functions, like 'gotoLine' and 'moveByLine' __DO_NOT__
-- evaluate 'flushLineEditor' or 'refillLineEditor' at all, allowing you to accumulate 'TextLine's
-- into the 'LineEditor' as you move it the cursor different lines in the 'TextBuffer'.
data LineEditor tags
  = LineEditor
    { theLineEditBuffer      :: !(UMVec.IOVector Char)
    , parentTextEditor       :: (TextBuffer tags)
      -- ^ A line editor can be removed from it's parent 'TextEditor'. A 'LineEditor' is defined
      -- such that it can only directly edit text in it's parent 'TextEditor'. A 'LineEditor' can
      -- indirectly edit text in any other parent, but only if a complete copy of the content of the
      -- line editor is made and passed it to some other 'TextEditor'.
    , theCharsBeforeCursor   :: !VecLength
    , theCharsAfterCursor    :: !VecLength
    , theCharCursorIsDefined :: !Bool
      -- ^ Set to 'True' if the char under the cursor is defined, 'False' otherwise.
    , theLineEditorIsClean   :: !Bool
      -- ^ This is set to 'True' if 'theBufferLineEditor' is a perfect copy of the 'TextLine'
      -- currently being referred to by the 'currentLineNumber'. If the content of
      -- 'theBufferLineEditor' has been modified by 'insertChar' or 'deleteChars', or if cursor has
      -- been moved to a different line, this field is set to 'False'.
    , theCursorBreakSize     :: !Word16
    , theLineEditorTags      :: tags
    }

instance IntSized (LineEditor tags) where { intSize = fromLength . lineEditorCharCount; }

-- Not for export: this buffer is formatted such that characters before the cursror are near index
-- zero, while characters after the cursor are near the final index.
lineEditBuffer :: Lens' (LineEditor tags) (UMVec.IOVector Char)
lineEditBuffer = lens theLineEditBuffer $ \ a b -> a{ theLineEditBuffer = b }

-- Not for export: this gets updated when inserting or deleting characters before the cursor.
charsBeforeCursor :: Lens' (LineEditor tags) VecLength
charsBeforeCursor = lens theCharsBeforeCursor $ \ a b -> a{ theCharsBeforeCursor = b }

-- Not for export: this gets updated when inserting or deleting characters after the cursor.
charsAfterCursor :: Lens' (LineEditor tags) VecLength
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
lineEditorTags :: Lens' (LineEditor tags) tags
lineEditorTags = lens theLineEditorTags $ \ a b -> a{ theLineEditorTags = b }

-- | This is set to 'True' if 'theBufferLineEditor' is a perfect copy of the 'TextLine' currently
-- being referred to by the 'currentLineNumber'. If the content of 'theBufferLineEditor' has been
-- modified by 'insertChar' or 'deleteChars', or if cursor has been moved to a different line, this
-- field is set to 'False'.
lineEditorIsClean :: Lens' (LineEditor tags) Bool
lineEditorIsClean = lens theLineEditorIsClean $ \ a b -> a{ theLineEditorIsClean = b }

-- | This is set to 'True' if the element under the cursor is defined, 'False' if the element under
-- the cursor is undefined.
charCursorIsDefined :: Lens' (LineEditor tags) Bool
charCursorIsDefined = lens theCharCursorIsDefined $ \ a b -> a{ theCharCursorIsDefined = b }

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
  return TextBufferState
    { theBufferDefaultLine   = TextLine
        { theTextLineTags      = tags
        , theTextLineString    = mempty
        , theTextLineBreakSize = 0
        }
    , theBufferLineBreaker   = lineBreakerNLCR
    , theBufferVector        = buf
    , theLinesAboveCursor    = 0
    , theLinesBelowCursor    = 0
    , theLineCursorIsDefined = False
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
newLineEditorIO :: TextBuffer tags -> tags -> IO (LineEditor tags)
newLineEditorIO parent tag = do
  buf <- UMVec.new 1024
  return LineEditor
    { theLineEditBuffer      = buf
    , parentTextEditor       = parent
    , theCharsBeforeCursor   = 0
    , theCharsAfterCursor    = 0
    , theCharCursorIsDefined = False
    , theLineEditorIsClean   = False
    , theCursorBreakSize     = 0
    , theLineEditorTags      = tag
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
lineEditorCharCount :: LineEditor tags -> Relative CharIndex
lineEditorCharCount ed = toLength $ fromLength $ theCharsBeforeCursor ed + theCharsAfterCursor ed

-- | This funcion returns the same value as 'textLineCursorSpan' but for a 'LineEditor'.
lineEditorUnitCount :: LineEditor tags -> TextCursorSpan
lineEditorUnitCount ed = TextCursorSpan $ fromLength $
  lineEditorCharCount ed - fromIntegral (theCursorBreakSize ed)

-- | Gets the 'lineEditorCharCount' value for the current 'LineEditor'.
getLineCharCount
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m (Relative CharIndex)
getLineCharCount = liftEditText $ lineEditorCharCount <$> use bufferLineEditor

-- | Gets the 'lineEditorUnitCount' value for the current 'LineEditor'.
getUnitCount
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m TextCursorSpan
getUnitCount = liftEditText $ lineEditorUnitCount <$> use bufferLineEditor

-- TODO: uncomment this, rewrite it
--
---- | Create a new 'LineEditor' by copying the line under the given 'TextLocation' point. The
---- 'LineEditor' can be updated with an 'EditLine' function. Note that this function works in any
---- monadic function type @m@ which instantiates 'Control.Monad.IO.Class.MonadIO', so this will work
---- in the @IO@ monad, the 'EditText' monad, the 'EditLine' monad, and in other contexts as well.
--newLineEditorAt
--  :: (MonadIO m
--     , Show tags --DEBUG
--     )
--  => TextBuffer tags -> TextLocation -> m (Either TextEditError (LineEditor tags))
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
--    let breakSize = line ^. textLineBreakSize
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
--      , parentTextEditor       = parent
--      , theCharsBeforeCursor   = cur
--      , theCharsAfterCursor    = strlen - cur - fromIntegral breakSize
--      , theLineEditorIsClean   = False
--      , theCharCursorIsDefined = False
--      , theCursorBreakSize     = breakSize
--      , theLineEditorTags      = theTextLineTags line
--      }

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

-- Not for export: requires correct accounting of line numbers to avoid segment faults.
linesAboveCursor :: Lens' (TextBufferState tags) VecLength
linesAboveCursor = lens theLinesAboveCursor $ \ a b -> a{ theLinesAboveCursor = b }

-- Not for export: requires correct accounting of line numbers to avoid segment faults.
linesBelowCursor :: Lens' (TextBufferState tags) VecLength
linesBelowCursor = lens theLinesBelowCursor $ \ a b -> a{ theLinesBelowCursor = b }

-- Not for export: indicates whether the line under the cursor is defined or undefined.
lineCursorIsDefined :: Lens' (TextBufferState tags) Bool
lineCursorIsDefined = lens theLineCursorIsDefined $ \ a b -> a{ theLineCursorIsDefined = b }

-- Not for export: the vector containing all the lines of text in this buffer.
bufferVector :: Lens' (TextBufferState tags) (MVec.IOVector (TextLine tags))
bufferVector = lens theBufferVector $ \ a b -> a{ theBufferVector = b }

-- | The current line of text being edited under the cursor.
bufferLineEditor :: Lens' (TextBufferState tags) (LineEditor tags)
bufferLineEditor = lens theBufferLineEditor $ \ a b -> a{ theBufferLineEditor = b }

-- | When moving the cursor up and down the 'TextBuffer' (e.g. using 'gotoPosition'), the cursor
-- should generally remain at the same character column position.
bufferTargetCol :: Lens' (TextBufferState tags) (Absolute CharIndex)
bufferTargetCol = lens theBufferTargetCol $ \ a b -> a{ theBufferTargetCol = b }

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
instance MonadEditText (FoldMapLines r fold) where { liftEditText = foldMapLiftEditText; }
instance MonadEditText (FoldLines r fold) where { liftEditText = foldLiftEditText; }

-- | This class is basically the same as 'MonadEditText', but lifts a 'EditLine' function rather
-- than an 'EditText' function. Note that 'MonadEditText' is a subclass of this typeclass, which
-- means if you 'liftEditChar' should work anywhere a 'liftEditText' function will work.
class MonadEditLine editor where
  liftEditLine
    :: (MonadIO m
       , Show tags --DEBUG
       )
    => EditLine tags m a -> editor tags m a

instance MonadEditLine EditLine where
  liftEditLine = id

instance MonadEditLine (FoldMapChars r fold) where
  liftEditLine = FoldMapChars . lift . lift . lift

instance MonadEditLine EditText where
  liftEditLine = editLine

instance MonadEditLine (FoldMapLines r fold) where
  liftEditLine = foldMapChars . liftEditLine

----------------------------------------------------------------------------------------------------

class Monad editor => GetLineBreakSize editor where
  getLineBreakSize :: editor VecLength

instance Monad m => GetLineBreakSize (EditLine tags m) where
  getLineBreakSize = VecLength . fromIntegral <$> use cursorBreakSize

instance GetLineBreakSize (ViewLine tags) where
  getLineBreakSize = VecLength . fromIntegral . theTextLineBreakSize <$> use viewerLine

----------------------------------------------------------------------------------------------------

-- | Push a 'TextLine' before or after the cursor. This function does not effect the content of the
-- 'bufferLineEditor'.
pushLine
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags -- DEBUG
     )
  => RelativeToCursor -> TextLine tags -> editor tags m ()
pushLine rel = liftEditText . pushElem rel

-- | Pop a 'TextLine' from before or after the cursor. This function does not effect the content of
-- the 'bufferLineEditor'. If you 'popLine' from 'Before' the cursor when the 'bufferLineEditor' is
-- at the beginning of the buffer, or if you 'popLine' from 'After' the cursor when the
-- 'bufferLineEditor' is at the end of the buffer, this function evaluates to an 'EndOfLineBuffer'
-- exception.
popLine
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => RelativeToCursor -> editor tags m (TextLine tags)
popLine = liftEditText . popElem

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
currentLineNumber = liftEditText $ cursorIndex >>= indexToAbsolute

-- | Get the current column number of the cursor.
currentColumnNumber
  :: (MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m (Absolute CharIndex)
currentColumnNumber = liftEditLine $ cursorIndex >>= indexToAbsolute

-- | Get the current cursor position. This function is identical to 'getPosition'.
currentTextLocation
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m TextLocation
currentTextLocation = liftEditText $
  TextLocation <$> currentLineNumber <*> editLine currentColumnNumber

copyLineEditor'
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => EditLine tags m (TextLine tags)
copyLineEditor' = TextLine <$> freezeVector <*> use cursorBreakSize <*> use lineEditorTags

-- | Create a copy of the 'bufferLineEditor'.
copyLineEditorText
  :: (MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m (TextLine tags)
copyLineEditorText = liftEditLine copyLineEditor'

-- | Returns 'True' if the given @'Absolute' 'CharIndex'@ value refers to any character after the
-- final non-line-breaking character in the current line, i.e. it points to any of the line-breaking
-- characters. If there are no line breaking characters on the current line, this function always
-- returns 'False'.
pointContainsLineBreak
  :: (MonadEditLine editor, MonadIO (editor tags m), MonadIO m, GetLineBreakSize (editor tags m)
     , Show tags --DEBUG
     )
  => Absolute CharIndex -> editor tags m Bool
pointContainsLineBreak pt = liftEditLine $ do
  lbrk <- getLineBreakSize
  if lbrk == 0 then return False else do
    pt   <- absoluteIndex pt
    size <- getAllocSize
    return $ lbrk /= 0 && pt > indexAfterRange 0 (size - lbrk)

-- | Create a 'TextLine' by copying the characters relative to the cursor.
copyChars
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Relative CharIndex -> EditLine tags m (TextLine tags)
copyChars rel = do
  relativeIndex rel >>= guardVecIndex (CharCountOutOfRange rel)
  break <- relativeToAbsolute rel >>= pointContainsLineBreak
  sliceFromCursor (unwrapRelative rel) >>= liftIO . UVec.freeze >>=
    makeLineWithSlice (if break then id else const 0)

-- not for export
makeLineWithSlice
  :: Monad m
  => (Word16 -> Word16) -> CharVector -> EditLine tags m (TextLine tags)
makeLineWithSlice onLbrk slice = do
  lbrk  <- use cursorBreakSize
  tags  <- use lineEditorTags
  return $ textLineBreakSize %~ onLbrk $ TextLine
    { theTextLineString    = slice
    , theTextLineTags      = tags
    , theTextLineBreakSize = lbrk
    }

-- | Calls 'copyChars' with a @'Relative' 'CharIndex'@ value equal to the number of characters
-- 'Before' or 'After' the cursor on the current line.
copyCharsToEnd
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => RelativeToCursor -> EditLine tags m (TextLine tags)
copyCharsToEnd rel = getSlice rel >>= liftIO . UVec.freeze >>= case rel of
  Before -> makeLineWithSlice (const 0)
  After  -> makeLineWithSlice id

-- | Create a 'TextLine' by copying the the characters in the given range from the line under the
-- cursor.
copyCharsRange
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Absolute CharIndex -> Absolute CharIndex -> EditLine tags m (TextLine tags)
copyCharsRange from to = do
  (from, to) <- (,) <$> validateIndex from <*> validateIndex to
  break <- pointContainsLineBreak to
  (,) <$> absoluteIndex from <*> absoluteIndex to >>= uncurry
    ( withRegion (const $ liftIO . UVec.freeze) $ \ a b -> liftIO $ do
        -- TODO: make this abstract so it can be reused
        let lenA = UMVec.length a
        let lenB = UMVec.length b
        vec <- UMVec.new (lenA + lenB)
        UMVec.copy (UMVec.slice    0 lenA vec) a
        UMVec.copy (UMVec.slice lenA lenB vec) b
        UVec.freeze vec
    ) >>=
    makeLineWithSlice (if break then id else const 0)

-- | Create a 'TextLine' by copying the the characters in between the two given indicies from the
-- line under the cursor. The characters on the two given indicies are included in the resulting
-- 'TextLine'.
copyCharsBetween
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => Absolute CharIndex -> Absolute CharIndex -> EditLine tags m (TextLine tags)
copyCharsBetween from to = copyCharsRange (min from to) (max from to)

-- | Read a 'TextLine' from an @('Absolute' 'LineIndex')@ address.
getLineIndex
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Absolute LineIndex -> editor tags m (TextLine tags)
getLineIndex = liftEditText . (validateIndex >=> absoluteIndex >=> getElemIndex)

-- | Write a 'TextLine' (as produced by 'copyLineEditorText' or getLineIndex') to an @('Absolute'
-- 'LineIndex')@ address.
putLineIndex
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Absolute LineIndex -> TextLine tags -> editor tags m ()
putLineIndex i line = liftEditText $ validateIndex i >>= absoluteIndex >>= flip putElemIndex line

-- | Replace the content in the 'bufferLineEditor' with the content in the given 'TextLine'. Pass
-- an integer value indicating where the cursor position should be set. This function does not
-- re-allocate the current line editor buffer unless it is too small to hold all of the characters
-- in the given 'TextLine', meaning this function only grows the buffer memory allocation, it never
-- shrinks the memory allocation.
refillLineEditor
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m ()
refillLineEditor = liftEditText $ getElem >>= refillLineEditorWith

-- | Like 'refillLineEditor', but replaces the content in the 'bufferLineEditor' with the content in
-- a given 'TextLine', rather the content of the current line.
refillLineEditorWith
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => TextLine tags -> editor tags m ()
refillLineEditorWith = \ case
  TextLineUndefined -> error
    "internal error: evaluated (refillLineEditorWith TextLineUndefined)"
  line              -> liftEditText $ do
    let myname   = "refillLineEditorWith"
    let srcvec = theTextLineString line
    let srclen = intSize srcvec
    let lbrksz = theTextLineBreakSize line
    let srctop = srclen - fromIntegral lbrksz
    cur <- max 0 . min srctop . charToIndex <$> use bufferTargetCol
    editLine $ do
      join $ maybe (pure ()) (void . modVector . const) <$> prepLargerVector srclen
      let targlolen = cur
      let targhilen = srclen - cur
      -- NOTE: must set before/after cursor count values BEFORE CALLING 'getLoSlice'
      --       and 'getHiSlice', because we need a slice of the size given here, and
      --       there is no guarantee that the line editor currently has the correct
      --       cursor size set to produce the slices of the correct size.
      charsBeforeCursor .= targlolen
      charsAfterCursor  .= targhilen
      -- NOTE: now we call 'getLoSlice and 'getHiSlice', which should produce slices
      --       of size 'targlolen' and 'targhilen'.
      targlo <- getSlice Before
      targhi <- getSlice After
      cursorBreakSize   .= lbrksz
      lineEditorTags    .= theTextLineTags line
      liftIO $ do
        UVec.copy targlo $ asrtSlice UnsafeOp myname
          asrtZero
          (asrtShow "length targlo" targlolen)
          ("srcvec", srcvec)
        UVec.copy targhi $ asrtSlice UnsafeOp myname
          (asrtShow "length targlo" targlolen)
          (asrtShow "length targhi" targhilen)
          ("srcvec", srcvec)
    bufferLineEditor . lineEditorIsClean .= True

-- | Delete the content of the 'bufferLineEditor' except for the line breaking characters (if any)
-- at the end of the line. This function does not change the memory allocation for the 'LineEditor',
-- it simply sets the character count to zero. Tags on this line are not effected.
clearLineEditor
  :: (MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m ()
clearLineEditor = liftEditLine $ do
  charsBeforeCursor .= 0
  use cursorBreakSize >>= assign charsAfterCursor . fromIntegral

-- | Like 'clearLineEditor', this function deletes the content of the 'bufferLineEditor', and tags
-- on this line are not effected. The difference is that this function replaces the
-- 'bufferLineEditor' with a new empty line, resetting the line editor buffer to the default
-- allocation size and allowing the garbage collector to delete the previous allocation. This means
-- the line editor buffer memory allocation may be shrunk to it's minimal/default size.
resetLineEditor
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m ()
resetLineEditor = liftEditText $ newLineEditor >>= assign bufferLineEditor

-- | Create a new 'LineEditor' value within the current 'EditText' context, using the default tags
-- given by 'bufferDefaultTags'. This function calls 'newLineEditorIO' using the value of
-- 'lineEditorTags'.
newLineEditor
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m (LineEditor tags)
newLineEditor = liftEditText $ thisTextBuffer >>= \ this ->
  use (bufferLineEditor . lineEditorTags) >>= liftIO . newLineEditorIO this

-- | Create a copy of the current 'lineEditBuffer' and return it. This function calls
-- 'copyLineEditorIO' using the value of the current 'lineEditBuffer'.
copyLineEditor
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m (LineEditor tags)
copyLineEditor = liftEditText $ use bufferLineEditor >>= liftIO . copyLineEditorIO

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
-- changing position. This can also be useful after accumulating the content of several lines of
-- text into the 'LineEditor'.
flushLineEditor
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m (TextLine tags)
flushLineEditor = liftEditText $ do
  clean <- use $ bufferLineEditor . lineEditorIsClean
  if clean
   then getElem Before 
   else do
    line <- editLine copyLineEditor'
    putElem Before line
    bufferLineEditor . lineEditorIsClean .= True
    return line

-- | Evaluate a function on the 'TextLine' currently under the 'currentLineNumber'. This function
-- throws an exception if the 'TextBuffer' is empty. __NOTE__ that the 'TextBuffer' is considered
-- empty if there are characters in the 'LineBuffer' which have not been flushed to the empty
-- 'TextBuffer' by either 'flushLineEditor' or 'flushRefill'.
withCurrentLine
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => (Absolute LineIndex -> TextLine tags -> editor tags m a) -> editor tags m a
withCurrentLine f = do
  (ln, line) <- liftEditText $ (,) <$> currentLineNumber <*> getElem Before
  case line of
    TextLineUndefined -> error $
      "internal error: "++show ln++" received from 'getLineIndex' points to undefined line"
    line              -> f ln line

-- | Return the number of lines of text in this buffer.
bufferLineCount
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m (Relative LineIndex)
bufferLineCount = liftEditText $ countToLine <$> countDefined

-- | Returns a boolean indicating whether there is no content in the current buffer.
bufferIsEmpty
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m Bool
bufferIsEmpty = bufferLineCount >>= \ ln ->
  if ln > 1 then return False
  else if ln <= 0 then return True
  else withCurrentLine $ \ _ line -> return $ textLineCursorSpan line == 0

-- | Return a pointer to the buffer currently being edited by the @editor@ function.
currentBuffer
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m (TextBuffer tags)
currentBuffer = liftEditText $ gets $ parentTextEditor . theBufferLineEditor

----------------------------------------------------------------------------------------------------

-- not for export
modifyColumn
  :: (MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => (Absolute CharIndex -> Absolute CharIndex -> Relative CharIndex)
  -> editor tags m (Absolute CharIndex)
modifyColumn f = liftEditLine $ do
  oldch  <- currentColumnNumber
  weight <- countDefined
  lbrksz <- fromIntegral <$> use cursorBreakSize
  shiftCursor $ Relative $ charToCount $ f oldch $ indexToChar $ weight - lbrksz
  indexToChar <$> indexNearCursor Before

-- | Usually when you move the cursor to a different line using 'gotoPosition', 'gotoLine',
-- 'moveByLine', or 'moveCursor', you expect the content of the 'LineEditor' to remain on the
-- current line and you expect the 'LineEditor' will begin editing the line at the new position to
-- where it moved -- but unless you evaluate your cursor motion with the 'flushRefill' function, this
-- is not the default cursor motion behavior. The 'LineEditor' must be explicitly instructed to
-- flush it's contents to the current line using 'flushLineEditor', and load the content of the new
-- line after moving using 'refillLineEditor'.
--
-- That is precisely what this function call does: it evaluates an @editor@ function (one that
-- usually evaluates a cursor motion), but before it does, it evaluate 'flushLineEditor', and after
-- evaluating 'flushRefill' it evaluates 'refillLineEditor'.
flushRefill
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m a -> editor tags m a
flushRefill motion = flushLineEditor >> motion <* refillLineEditor

-- | This function calls 'moveByLine' and then 'moveByChar' to move the cursor by a number of lines
-- and characters relative to the current cursor position.
--
-- __WARNING__: This evaluates 'flushLineEditor' before moving the cursor, and evaluates
-- 'refillLineEditor' after moving the cursor. This is necessary because it is impossible to
-- instruct the 'LineEditor' which character position to move to unless it contains an accurate copy
-- of the line of the buffer that it is currently editing.
--
-- If you want to move the cursor without flushing and refilling, use 'gotoLine' or
-- 'moveByLine'. Then once you have decided what to do with the content of the current line editor,
-- can move the cursor column with 'gotoChar' or 'moveByChar', or set the new target column with the
-- expression @('bufferTargetCol' 'Control.Lens..=' c)@.
moveCursor
  :: (MonadEditText editor, MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Relative LineIndex -> Relative CharIndex -> editor tags m TextLocation
moveCursor row col = TextLocation <$> flushRefill (moveByLine row) <*> moveByChar col

-- | Move the cursor to a different line by an @n :: Int@ number of lines. A negative @n@ indicates
-- moving the cursor toward the start of the buffer, a positive @n@ indicates moving the cursor
-- toward the end of the buffer.
--
-- This function __DOES NOT__ evaluate 'flushLineEditor' or 'refillLineEditor'.
moveByLine
  :: forall editor tags m . (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Relative LineIndex -> editor tags m (Absolute LineIndex)
moveByLine rel = liftEditText $ do
  liftEditText . shiftCursorChk . Relative . lineToCount $ rel
  currentLineNumber

-- | Move the cursor to a different character position within the 'bufferLineEditor' by an @n ::
-- Int@ number of characters. A negative @n@ indicates moving toward the start of the line, a
-- positive @n@ indicates moving toward the end of the line.
--
-- This function does not wrap the cursor if motion moves past the end or beginning of the line, to
-- do this, evaluate 'moveByCharWrap'.
--
-- This function __DOES NOT__ evaluate 'flushLineEditor' or 'refillLineEditor'.
moveByChar
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Relative CharIndex -> editor tags m (Absolute CharIndex)
moveByChar count = liftEditText $ do
  cur <- currentColumnNumber
  bufferTargetCol .= shiftAbsolute cur count
  modifyColumn $ \ column top ->
    if count > 0 then min count $ diffAbsolute column top
    else if count < 0 then max count $ diffAbsolute column 1
    else 0
 
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
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => TextCursorSpan -> editor tags m TextLocation
moveByCharWrap dist = liftEditText $
  currentTextLocation >>= flip spanDistance dist >>= gotoPosition . fst

-- | Go to an absolute line number, the first line is 1 (line 0 and below all send the cursor to
-- line 1), the last line is 'Prelude.maxBound'.
--
-- This function __DOES NOT__ evaluate 'flushLineEditor' or 'refillLineEditor'.
gotoLine
  :: (MonadEditText editor, MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Absolute LineIndex -> editor tags m (Absolute LineIndex)
gotoLine ln0 = liftEditText $ do
  let ln = lineToIndex ln0
  cur   <- indexNearCursor Before
  count <- countDefined
  i <- moveByLine $ countToLine $ max 0 (min ln count) - cur
  use bufferTargetCol >>= gotoChar
  return i

-- | Go to an absolute character (column) number, the first character is 1 (character 0 and below
-- all send the cursor to column 1), the last line is 'Prelude.maxBound'.
--
-- This function __DOES NOT__ evaluate 'flushLineEditor' or 'refillLineEditor'.
gotoChar
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Absolute CharIndex -> editor tags m (Absolute CharIndex)
gotoChar ch = liftEditText $ do
  bufferTargetCol .= ch
  modifyColumn $ \ column top -> diffAbsolute column $ max 1 $ min top ch

----------------------------------------------------------------------------------------------------

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
  if isBreak c then return 0 else do
    liftEditLine (pushElem rel c)
    bufferLineEditor . lineEditorIsClean .= False
    return 1

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
   { theTextLineString    = line
   , theTextLineBreakSize = lbrksz
   , theTextLineTags      = tagsB
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

-- | This function only deletes characters on the current line, if the cursor is at the start of the
-- line and you evaluate @'deleteChars' 'Before'@, this function does nothing. The sign of the
-- 'CharIndex' given will determine the direction of travel for the deletion -- negative will delete
-- moving toward the beginning of the line, positive will delete moving toward the end of the
-- line. This function never deletes line breaking characters, even if you delete toward the end of
-- the line. This function returns the number of characters actually deleted as a negative number
-- (or zero), indicating a change in the number of characters in the buffer.
deleteChars
  :: (MonadEditLine editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => TextCursorSpan -> editor tags m CharStats
deleteChars req@(TextCursorSpan n) =
  let done count = return
        CharStats
        { cursorStepCount = signum req * TextCursorSpan count
        , deltaCharCount  = Relative $ CharIndex $ negate count
        }
  in liftEditLine $
  if n < 0 then do -- TODO: make this code less copy-pastey
    before <- indexNearCursor Before
    let count = negate $ min before $ abs n 
    modCount Before $ const $ before + count
    when (count /= 0) $ lineEditorIsClean .= False
    done count
  else if n > 0 then do
    lbrksz <- fromIntegral <$> use cursorBreakSize
    after  <- indexNearCursor After
    let count = negate $ min n $ max 0 $ after - lbrksz
    modCount After $ const $ after + count
    when (count /= 0) $ lineEditorIsClean .= False
    done count
  else done 0

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

deleteAllChars
  :: ( MonadIO m
     , Show tags --DEBUG
     )
  => RelativeToCursor -> EditText tags m CharStats
deleteAllChars = \ case
  Before -> error "TODO: deleteAllChars Before"
  After  -> error "TODO: deleteAllChars After"

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
          liftEditLine $ overwriteAtCursor direction const keep
          let st = st0 <>
                CharStats
                { cursorStepCount = sign $ textLineCursorSpan delete
                , deltaCharCount  = countToChar $ negate $ intSize delete
                } 
          put st
        return []

-- | This function evaluates the 'lineBreaker' function on the given string, and beginning from the
-- current cursor position, begins inserting all the lines of text produced by the 'lineBreaker'
-- function.
insertString
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => String -> editor tags m (Relative CharIndex)
insertString str = liftEditText $ do
  breaker <- use (bufferLineBreaker . lineBreaker)
  let writeStr = editLine . fmap sum . mapM ((>> (return 1)) . pushElem Before)
  let writeLine (str, lbrk) = do
        (strlen, lbrklen) <- (,) <$> writeStr str <*> writeStr lbrk
        let maxlen = fromIntegral (maxBound :: Word16) :: Int
        if lbrklen >= maxlen
         then error $ "insertString: line break string length is "++show lbrklen++
                "exceeeds maximum length of "++show maxlen++" characters"
         else do
          line <- editLine $ do
            cursorBreakSize .= fromIntegral lbrklen
            copyLineEditor' <* (charsBeforeCursor .= 0 >> charsAfterCursor .= 0)
          when (lbrklen > 0) $ pushElem Before line
          bufferLineEditor . lineEditorIsClean .= (lbrklen > 0)
          return (strlen + lbrklen)
  let loop count = seq count . \ case
        []        -> return count
        line:more -> writeLine line >>= flip loop more
  count <- Relative . CharIndex <$> loop 0 (breaker str)
  --editLine copyLineEditor' >>= pushElem Before -- shouldn't be necessary
  return count

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
      liftEditLine $ pushElemVec Before lbrk
      cursor <- use bufferLineEditor
      let vec = cursor ^. lineEditBuffer
      let cur = cursor ^. charsBeforeCursor
      str <- liftIO $! UVec.freeze $! asrtMSlice SafeOp "lineBreak"
        asrtZero (asrtShow "cur" cur) ("lineEditBuffer", vec)
      pushLine Before $ TextLine
        { theTextLineTags      = cursor ^. lineEditorTags
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
        str <- liftIO $! UVec.freeze $!
          asrtMSlice SafeOp "lineBreak"
          (asrtShow "len-cur" $ len - cur)
          (asrtShow "cur" cur)
          ("lineEditBuffer", vec)
        pushLine After $ TextLine
          { theTextLineTags      = cursor ^. lineEditorTags
          , theTextLineString    = str
          , theTextLineBreakSize = lbrkSize
          }
        bufferLineEditor . charsAfterCursor .= 0
      liftEditLine $ pushElemVec After lbrk

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

----------------------------------------------------------------------------------------------------

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

-- | Get the current position of the cursor. This function is identical to 'currentTextLocation'.
getPosition
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m TextLocation
getPosition = currentTextLocation

-- | This function calls 'gotoLine' and then 'gotoChar' to move the cursor to an absolute a line
-- number and characters (column) number.
--
-- __WARNING__: This evaluates 'flushLineEditor' before moving the cursor, and evaluates
-- 'refillLineEditor' after moving the cursor. This is necessary because it is impossible to
-- instruct the 'LineEditor' which character position to move to unless it contains an accurate copy
-- of the line of the buffer that it is currently editing.
--
-- If you want to move the cursor without flushing and refilling, use 'gotoLine' or
-- 'moveByLine'. Then once you have decided what to do with the content of the current line editor,
-- can move the cursor column with 'gotoChar' or 'moveByChar', or set the new target column with the
-- expression @('bufferTargetCol' 'Control.Lens..=' c)@.
gotoPosition
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => TextLocation -> editor tags m TextLocation
gotoPosition (TextLocation{theLocationLineIndex=ln,theLocationCharIndex=ch}) = liftEditText $
  TextLocation <$> flushRefill (gotoLine ln) <*> gotoChar ch

-- | Save the location of the cursor, then evaluate an @editor@ function. After evaluation
-- completes, restore the location of the cursor (within range, as the location may no longer exist)
-- and return the result of evaluation.
saveCursorEval
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => editor tags m a -> editor tags m a
saveCursorEval f = do
  (cur, a) <- (,) <$> currentLineNumber <*> f
  liftEditText (gotoLine cur) >> return a

class RelativeToAbsoluteCursor index editor | editor -> index where
  -- | Convert a 'Relative' index (either a 'LineIndex' or 'CharIndex') to an 'Absolute' index.
  relativeToAbsolute :: Relative index -> editor (Absolute index)

instance
  (MonadIO m
  , Show tags --DEBUG
  ) => RelativeToAbsoluteCursor LineIndex (EditText tags m) where
  relativeToAbsolute = liftEditText .
    relativeIndex . fmap lineToCount >=> fmap (fmap indexToLine) . absoluteIndex

instance
  (MonadIO m
  , Show tags --DEBUG
  ) => RelativeToAbsoluteCursor CharIndex (EditLine tags m) where
  relativeToAbsolute = liftEditLine .
    relativeIndex . fmap charToCount >=> fmap (fmap indexToChar) . absoluteIndex

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

-- | Decompose a 'TextView' into a list of 'TextLine's.
textViewToList :: TextView tags -> [TextLine tags]
textViewToList = Vec.toList . textViewVector

-- | Decompose a 'TextView' into a list of tagged 'String's. The 'String' is paired with a 'Word16'
-- count of the number of line-breaking characters exist at the end of the string, and the @tags@
-- assoicated with the text.
textViewToStrings :: TextView tags -> [(String, (Word16, tags))]
textViewToStrings = textViewToList >=> \ case
  TextLineUndefined -> []
  TextLine
   { theTextLineString=vec
   , theTextLineTags=tags
   , theTextLineBreakSize=lbrksiz
   } -> [(unpack vec, (lbrksiz, tags))]

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
              let listA = Vec.toList $ asrtSlice SafeOp myname
                    asrtZero (asrtShow "lenA-1" $ lenA - 1) ("vecA", vecA)
              let listB = Vec.toList $ asrtSlice SafeOp myname
                    asrtOne  (asrtShow "lenB-1" $ lenB - 1) ("vecB", vecB)
              let (size, list) = if theTextLineBreakSize lastA > 0
                    then (lenAB, listA ++ lastA : firstB : listB)
                    else (lenAB - 1, listA ++ lineAB : listB)
              newVec <- MVec.new size
              forM_ (zip [0 ..] list) $ \ (i, elem) ->
                asrtMWrite UnsafeOp myname ("newVec", newVec) (asrtShow "i" i) elem
              return newVec
          )
    }
  where { myname = "textViewAppend"; }

-- | Copy a region of the current 'TextBuffer' into a 'TextView', delimited by the two given
-- 'TextLocation' values. If the two 'TextLocation' values given are identical, an empty 'TextView'
-- is constructed. The 'TextLocation' value further from the start of the 'TextBuffer' is considered
-- to be the end point of the text to be copied into the resulting 'TextView', and the character
-- under the end point is not included in the resulting 'TextView'.
textView
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => TextLocation -> TextLocation -> editor tags m (TextView tags)
textView from0 to0 = liftEditText $ do
  let myname = "textView"
  (from, loline) <- validateLocation $ min from0 to0
  (to,   hiline) <- validateLocation $ max from0 to0
  if (from ^. lineIndex) == (to ^. lineIndex) then return $
    let numchars = diffAbsolute (from ^. charIndex) (to ^. charIndex) in TextView
      { textViewCharCount = charToCount numchars
      , textViewVector = Vec.singleton $ sliceLineNoChk (from ^. charIndex) numchars loline
      }
    else do
      to <- if to ^. charIndex > 1 then pure to else
        fst <$> validateLocation (to & (lineIndex -~ 1) . (charIndex .~ maxBound))
      -- Compute the range of lines to copy.
      let top = lineToCount $ diffAbsolute (from ^. lineIndex) (to ^. lineIndex)
      newvec <- copyRegion (Absolute $ lineToIndex $ from ^. lineIndex) (Relative $ top + 1)
      let freeze = liftIO . if unsafeMode then Vec.unsafeFreeze else Vec.freeze
      newvec <- liftIO $ do
        asrtMWrite UnsafeOp myname ("newvec", newvec) asrtZero $
          snd $ splitLineAt (from ^. charIndex) loline
        asrtMWrite UnsafeOp myname ("newvec", newvec) (asrtShow "top" top) $
          fst $ splitLineAt (to   ^. charIndex) hiline
        freeze newvec
      return TextView
        { textViewCharCount = sum $ intSize <$> Vec.toList newvec
        , textViewVector    = newvec
        }

-- | Like 'textView', creates a new text view, but rather than taking two 'TextLocation's to delimit
-- the range, takes two @('Absolute' 'LineIndex')@ values to delimit the range.
textViewOnLines
  :: (MonadEditText editor, MonadIO (editor tags m), MonadIO m
     , Show tags --DEBUG
     )
  => Absolute LineIndex -> Absolute LineIndex -> editor tags m (TextView tags)
textViewOnLines from to = textView
  (TextLocation
   { theLocationLineIndex = min from to
   , theLocationCharIndex = Absolute $ CharIndex 1
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
  forM_ (zip idx $ Vec.toList vec) $ \ (i, elem) ->
    asrtMWrite UnsafeOp "newTextBufferFromView" ("newBuf", newBuf) (asrtShow "i" i) elem
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
      -- this function returns the 'TextLocation' of the cursor, similar to how
      -- 'currentTextLocation' returns the position of the text editing cursor.
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
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => TextLocation -> TextLocation -> EditText tags m (StreamCursor tags)
newStreamCursorRange start end = streamResetCache StreamCursor
  { theStreamCache    = TextLineUndefined
  , theStreamLocation = min start end
  , theStreamEndpoint = max start end
  } >>= streamResetEndpoint (max start end)

-- | A convenience function that calls 'newStreamCursorRange' with 'minBound' as the 'TextLocation'.
newStreamCursor
  :: (MonadIO m
     , Show tags --DEBUG
     )
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
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => StreamCursor tags -> TextLocation -> EditText tags m (StreamCursor tags)
streamGoto cursor = validateLocation >=> \ (loc, txt) ->
  return cursor{ theStreamCache = txt, theStreamLocation = loc }

-- | Thinking of the 'StreamCursor' as a cursor pointing to a character within a 'TextBuffer' this
-- function returns the character currently under the cursor __without advancing the cursor.__ This
-- function throws an @'EndOfLineBuffer' 'After'@ exception if 'streamResetCache' cannot resolve
-- 'theStreamLocation'. This function throws an @'EndOfCharBuffer' 'After'@ exception if cursor has
-- somehow moved beyond 'theStreamCache', which could only happen if the 'streamResetCache' function
-- has not been called after transplanting the 'StreamCursor' to another 'TextBuffer'.
streamLook
  :: (MonadIO m
     , Show tags --DEBUG
     )
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
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => StreamCursor tags -> EditText tags m (StreamCursor tags)
streamStep s = do
  let i   = charToIndex $ s ^. streamLocation . charIndex
  let top = intSize     $ s ^. streamCache
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
  line              -> charToIndex (s ^. streamLocation . charIndex) >= intSize line
{-# INLINE streamIsEOL #-}

-- | Tests whether 'theStreamLocation' is greater-than or equal-to 'theStreamEndpoint'.
streamIsEOF :: StreamCursor tags -> Bool
streamIsEOF s = theStreamLocation s >= theStreamEndpoint s
{-# INLINE streamIsEOF #-}

-- | There are times when the 'StreamCursor' contains a cached 'TextLine' (given by the
-- 'streamCache' value) that is different from the actual 'TextLine' unders the cursor
-- position of the 'TextLocation' given by 'streamLocation'. This can happen when you evaluate
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
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => StreamCursor tags -> EditText tags m (StreamCursor tags)
streamResetCache s = do
  (loc, txt) <- validateLocation (theStreamLocation s)
  return (s & streamLocation .~ loc & streamCache .~ txt)

-- | This function should only need to be evaluated once as long as the 'TextBuffer' never changes
-- suring the parsing process. If the 'TextBuffer' does change while the parsing has been paused,
-- this function must be evaluated to ensure the 'StreamCursor' is aware of the endpoint.
streamResetEndpoint
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => TextLocation -> StreamCursor tags -> EditText tags m (StreamCursor tags)
streamResetEndpoint loc s = do
  (loc, _txt) <- validateLocation loc
  return (s & streamLocation .~ loc)

-- | You can use the 'streamTags' lens to alter the @tags@ value of the line cached (the line
-- returned by 'streamCache'd function), but to actually store these changes back to the
-- 'TextBuffer', this function must be called. This function is called automatically by the
-- 'streamStep' function when the cursor steps past the end of the current line and to the
-- next line.
streamCommitTags
  :: (MonadIO m
     , Show tags --DEBUG
     )
  => StreamCursor tags -> EditText tags m ()
streamCommitTags s = putLineIndex (s ^. streamLocation . lineIndex) (s ^. streamCache)

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

-- | Print debugger information about the structured data that forms the 'TextView' to standard
-- output. __WARNING:__ this print's every line of text in the view, so if your text view has
-- thousands of lines of text, there will be a lot of output.
debugPrintView :: (Show tags, MonadIO m) => (String -> IO ()) -> TextView tags -> m ()
debugPrintView output view = do
  (_, errCount) <- forLinesInView view (1 :: Int, 0 :: Int) $ \ _halt line -> do
    lineNum <- state $ \ (lineNum, errCount) ->
      (lineNum, (lineNum + 1, errCount + if textLineIsUndefined line then 1 else 0))
    liftIO $ output $ ralign lineNum ++ ": " ++ show line
  liftIO $ output ""
  when (errCount > 0) $ error $ "iterated over "++show errCount++" undefined lines"

-- | Print debugger information about the structured data that forms the 'TextBuffer' to standard
-- output. __WARNING:__ this print's every line of text in the buffer, so if your text buffer has
-- thousands of lines of text, there will be a lot of output.
debugPrintBuffer
  :: (MonadEditText editor, Show tags, MonadIO (editor tags m), MonadIO m)
  => (String -> IO ()) -> editor tags m ()
debugPrintBuffer output = liftEditText $ do
  lineVec <- use bufferVector
  let len = MVec.length lineVec
  let printLines nullCount i = if i >= len then return () else do
        line <- liftIO $
          asrtMRead UnsafeOp "debugPrintBuffer" ("lineVec", lineVec) (asrtShow "i" i)
        let showLine = output $ ralign i ++ ": " ++ show line
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
  :: (MonadEditText editor, Show tags, MonadIO (editor tags m), MonadIO m)
  => (String -> IO ()) -> editor tags m ()
debugPrintCursor output = liftEditText $ do
  cur <- use bufferLineEditor
  let charVec    = theLineEditBuffer cur
  let charVecLen = UMVec.length charVec
  let before     = cur ^. charsBeforeCursor
  let after      = cur ^. charsAfterCursor
  liftIO $ do
    str <- forM [0 .. charVecLen - 1] $
      asrtMRead UnsafeOp "debugPrintCursor" ("charVec", charVec) . (,) "i"
    output $ "     bufferLineEditor: " ++ show str
    output $ "       lineEditorTags: " ++ show (cur ^. lineEditorTags)
    output $ " __cursorVectorLength: " ++ show charVecLen
    output $ "    charsBeforeCursor: " ++ show before
    output $ "     charsAfterCursor: " ++ show after
    output $ "      cursorInputSize: " ++ show (before + after)
    output $ "  cursorLineBreakSize: " ++ show (theCursorBreakSize cur)
