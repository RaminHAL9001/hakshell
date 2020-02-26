-- | This module implements a generic, mutable vector with a cursor. Moving the cursor shifts
-- elements after the cursor toward the end of the vector, ensuring pushing elements is always an
-- O(1) operation as long as there is space available. Moving the cursor is an O(n) operation, where
-- /n/ is the distance of the cursor movement.
module Hakshell.GapBuffer
  ( IsIndex(..), IsLength(..), HasOpposite(..),
    VecIndex(..), VecLength(..),
    indexAfterRange, indexAtRangeEnd, indexDistance, indexRange, distanceFromOrigin,
    Relative(..), RelativeToCursor(..), relative, unwrapRelative,
    Absolute(..), absoluteIndex, indexToAbsolute,
    MonadEditVec(..), MonadGapBuffer(..), getVector,
    indexNearCursor, cursorIndex, distanceFromCursor, cursorAtEnd, relativeIndex,
    testIndex, validateIndex, validateLength,
    countOnCursor, countDefined, countUndefined,
    getVoid, sliceFromCursor, sliceBetween, safeSliceBetween, withVoidSlice, getSlice,
    UnsafeSlice, safeFreeze, safeClone, mutableCopy, safeCopy, sliceSize,
    withFullSlices, withRegion, withNewVector, copyRegion,
    putElemIndex, getElemIndex, putElem, getElem, delElem,
    pushElem, pushElemVec, popElem, shiftCursor, moveCursorBy, moveCursorTo,
    minPow2ScaledSize, prepLargerVector, growVector, freezeVector, copyVec,
    ----
    GapBuffer(..), GapBufferState(..), BufferError(..), gapBufferLength,
    runGapBufferNew, runGapBuffer, cloneGapBufferState,
    gapBufferBeforeCursor, gapBufferAfterCursor, gapBufferCursorIsDefined, gapBufferVector,
    IIBuffer(..), IIBufferState(..),
    iiBufferCursor, iiBufferVector, runIIBuffer, gapBufferLiftIIBuffer,
  ) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State

--import qualified Data.Vector                 as Vec
--import qualified Data.Vector.Mutable         as MVec
--import qualified Data.Vector.Unboxed.Base    as UBox
--import qualified Data.Vector.Unboxed.Mutable as UMVec
--import qualified Data.Vector.Unboxed         as UVec
import qualified Data.Vector.Generic         as GVec
import qualified Data.Vector.Generic.Mutable as GMVec
import qualified Data.Vector.Generic.New     as GNew

----------------------------------------------------------------------------------------------------

-- Any vector operations that have a safe (bounds-chekced) version and an unsafe version will be
-- switched to the unsafe version when this constant is set to True.
unsafeMode :: Bool
unsafeMode = False

-- Set this to 'True' to install additional checks throughout this module, especially after having
-- made a change to any function that manipulates mutable arrays.
enableAssertions :: Bool
enableAssertions = True

----------------------------------------------------------------------------------------------------

-- An assertion system.

-- Check that 'Assertion's are true. If one or more 'Assertion's are false, report which ones are
-- false.
assert :: FunctionName -> [Assertion] -> r -> r
assert funcName tests a =
  if not enableAssertions || and (checkAssertion <$> tests) then a else error $ 
  "-- | "++funcName++": ASSERTION FAILED\n"++
  unlines (("-- | " ++) <$> (tests >>= reportAssertion))
{-# INLINE assert #-}

type FunctionName = String

type LabeledValue a = (String, a)

data Assertion = Info String | Assertion String Bool

data UseSafeOp = UnsafeOp | SafeOp
  deriving (Eq, Ord, Show)

checkAssertion :: Assertion -> Bool
checkAssertion = \ case { Info{} -> True; Assertion _ ok -> ok; }

reportAssertion :: Assertion -> [String]
reportAssertion = \ case { Info msg -> [msg]; Assertion msg ok -> if ok then [] else [msg]; }

-- TODO: wrap this function up in a MonadEditVec function
asrtGReadWrite
  :: (GMVec.MVector vec elem)
  => (vec st elem -> Int)
  -> (vec st elem -> Int -> rw)
  -> (vec st elem -> Int -> rw)
  -> UseSafeOp -> FunctionName
  -> LabeledValue (vec st elem) -> LabeledValue VecIndex -> rw
asrtGReadWrite length safe unsafe safety funcName (veclbl, vec) (ilbl, VecIndex i) =
  let len = ("(length "++veclbl++")", length vec) in
  assert funcName
    [asrtZero `le` (ilbl,i), (ilbl,i) `lt` len]
    ((if not unsafeMode || safety == SafeOp then safe else unsafe) vec i)
{-# INLINE asrtGReadWrite #-}

-- TODO: place this functionality into 'getElemIndex' and all calls to this function should be
-- replaced with calls to 'getElemIndex'.
asrtMRead
  :: (PrimMonad m, GMVec.MVector vec elem)
  => UseSafeOp -> FunctionName
  -> LabeledValue (vec (PrimState m) elem) -> LabeledValue VecIndex -> m elem
asrtMRead = asrtGReadWrite GMVec.length GMVec.read GMVec.unsafeRead
{-# INLINE asrtMRead #-}

-- TODO: place this functionality into 'putElemIndex' and all calls to this function should be
-- replaced with calls to 'putElemIndex'.
asrtMWrite
  :: (PrimMonad m, GMVec.MVector vec elem)
  => UseSafeOp -> FunctionName
  -> LabeledValue (vec (PrimState m) elem) -> LabeledValue VecIndex -> elem -> m ()
asrtMWrite = asrtGReadWrite GMVec.length GMVec.write GMVec.unsafeWrite
{-# INLINE asrtMWrite #-}

type AssertCompare a = LabeledValue a -> LabeledValue a -> Assertion

asrtCompare :: String -> (a -> a -> Bool) -> AssertCompare a
asrtCompare compareName compare (lblA, a) (lblB, b) =
  Assertion ('(' : lblA ++ ' ' : compareName ++ ' ' : lblB ++ ")") (compare a b)

asrtShow :: Show a => String -> a -> LabeledValue a
asrtShow varName val = if null varName then (show val, val) else
  ('(' : varName ++ '=' : show val ++ ")", val)

--eq :: Eq a => AssertCompare a
--eq = asrtCompare "==" (==)

--ne :: Ord a => AssertCompare a
--ne = asrtCompare "/=" (/=)

--gt :: Ord a => AssertCompare a
--gt = asrtCompare ">"  (>)

--ge :: Ord a => AssertCompare a
--ge = asrtCompare ">=" (>=)

lt :: Ord a => AssertCompare a
lt = asrtCompare "<"  (<)

le :: Ord a => AssertCompare a
le = asrtCompare "<=" (<=)

asrtZero :: Num n => LabeledValue n
asrtZero = ("0", 0)

----------------------------------------------------------------------------------------------------

modifyAndUse :: MonadState st m => Lens' st a -> (a -> a) -> m a
modifyAndUse lens = (lens %=) >=> \ () -> use lens

----------------------------------------------------------------------------------------------------

class IsIndex  i where { fromIndex  :: i -> Int; toIndex  :: Int -> i; }
class IsLength i where { fromLength :: i -> Int; toLength :: Int -> i; }

----------------------------------------------------------------------------------------------------

class HasOpposite a where { opposite :: a -> a; }
instance HasOpposite Bool where { opposite = not; }
instance HasOpposite Int  where { opposite = negate; }
instance HasOpposite Integer where { opposite = negate; }
instance HasOpposite (Either a a) where
  opposite = \ case { Right a -> Left a; Left a -> Right a; }
instance HasOpposite RelativeToCursor where
  opposite = \ case { Before -> After; After -> Before }

----------------------------------------------------------------------------------------------------

-- | Used for indexing lines and characters relative to the cursor.
newtype Relative a = Relative a
  deriving (Eq, Ord, Show, Read, Enum, Num, Bounded, Functor)

instance Num a => Semigroup (Relative a) where
  (Relative a) <> (Relative b) = Relative (a + b)

instance Num a => Monoid (Relative a) where
  mappend (Relative a) (Relative b) = Relative (a + b)
  mempty = Relative 0

-- | Used for indexing absolute lines and characters (relative to the start of the document, which
-- is line 1).
newtype Absolute a = Absolute a
  deriving (Eq, Ord, Show, Read, Enum, Num, Bounded)

-- | Controls whether characters are inserted/deleted before or after the cursor.
data RelativeToCursor = Before | After
  deriving (Eq, Ord, Read, Show, Bounded, Enum)

unwrapRelative :: IsLength i => i -> VecLength
unwrapRelative = toLength . fromLength

relative :: IsLength i => VecLength -> i
relative = toLength . fromLength

----------------------------------------------------------------------------------------------------

-- | Used mostly internally to indicate an index that can be used to access an element in the vector
-- of a gap buffer, as opposed to an index relative to the cursor, or relative to the gap.
newtype VecIndex = VecIndex Int
  deriving (Eq, Ord, Show, Read, Enum, Num, Bounded)

instance IsIndex VecIndex where -- TODO: delete this?
  fromIndex (VecIndex i) = i
  toIndex = VecIndex

-- | Used mostly internally to indicate the size of a vector of a gap buffer, or the size of a slice
-- of a the vector of a gap buffer, as opposed to an index relative to the cursor, or relative to
-- the gap.
newtype VecLength = VecLength Int
  deriving (Eq, Ord, Show, Read, Enum, Num, Bounded)

instance Semigroup VecLength where { (<>) = (+); }
instance Monoid    VecLength where { mempty = 0; mappend = (<>); }
instance IsLength  VecLength where -- TODO: delete this?
  fromLength (VecLength i) = i
  toLength = VecLength

-- | The first index after the range specified.
indexAfterRange :: VecIndex -> VecLength -> VecIndex
indexAfterRange (VecIndex i) (VecLength len) = VecIndex (i + len)

-- | The final index of the range specified
indexAtRangeEnd :: VecIndex -> VecLength -> VecIndex
indexAtRangeEnd i0 len = let (VecIndex i) = indexAfterRange i0 len in VecIndex (i - 1)

-- | Compute the distance of the first index parameter __relative to__ the second index
-- parameter. This function is similar to the function 'Prelude.subtract'. See also 'indexRange'
-- which is similar to this function but used in situations where you want to cover a range of
-- indicies, rather than compute the distance between indicies.
indexDistance :: VecIndex -> VecIndex -> VecLength
indexDistance (VecIndex a) (VecIndex b) = VecLength $ subtract a b

-- | Compute the length __covered by the range of indicies__ between two 'VecIndex' values. Unline
-- 'indexDistance' this function always returns a positive (non-zero) number because it is designed
-- to produce a 'VecLength' value that selects all elements between two delimiting indicies,
-- including the elements at the delimited indicies.
indexRange :: VecIndex -> VecIndex -> VecLength
indexRange (VecIndex a) (VecIndex b) = VecLength $ 1 + abs (subtract a b)

-- | Returng a 'VecLength' indicating the distance the given 'VecIndex' is from the beginning of the
-- buffer.
distanceFromOrigin :: VecIndex -> VecLength
distanceFromOrigin (VecIndex a) = VecLength a

----------------------------------------------------------------------------------------------------

-- | This is an opaque wrapper which is used to wrap unsafe copies of vectors, these are duplicates
-- of a pointer into the vector, but not a deep-copy of the elements. This is unsafe because it can
-- lead to null-pointer exceptions. To make it safer, we make use of Haskell's type system to
-- restrict the functions in which it can be used.
newtype UnsafeSlice vec = UnsafeSlice { unwrapUnsafeSlice :: vec }

sliceSize :: (vec -> Int) -> UnsafeSlice vec -> VecLength
sliceSize length (UnsafeSlice vec) = VecLength $ length vec

safeFreeze
  :: (PrimMonad m, GVec.Vector vec elem)
  => UnsafeSlice (GVec.Mutable vec (PrimState m) elem) -> m (vec elem)
safeFreeze (UnsafeSlice vec) = GVec.freeze vec

--mutableClone
--  :: (PrimMonad m, GMVec.MVector vec elem)
--  => UnsafeSlice (vec (PrimState m) elem)
--  -> SafeEvalSlice m (vec (PrimState m) elem)
--mutableClone (UnsafeSlice vec) = liftIO (GMVec.clone vec)

safeClone :: GVec.Vector vec elem => UnsafeSlice (vec elem) -> vec elem
safeClone (UnsafeSlice vec) = runST (GNew.run (GVec.clone vec) >>= GVec.freeze)

-- | The expression @('mutableCopy' a b)@ copies the elements from @b@ into the elements of @a@,
-- overwriting what was in @a@ before, and leaving @b@ unchanged.
mutableCopy
  :: (PrimMonad m, GMVec.MVector vec elem)
  => UnsafeSlice (vec (PrimState m) elem)
  -> UnsafeSlice (vec (PrimState m) elem)
  -> m ()
mutableCopy (UnsafeSlice a) (UnsafeSlice b) = GMVec.copy a b

-- | The expression @('mutableCopy' a b)@ copies the elements from an immutable buffer @b@ into the
-- mutable elements of @a@, overwriting what was in @a@ before. This is similar to 'mutableCopy' but
-- takes an immutable input vector instead of a mutable one.
safeCopy
  :: (PrimMonad m, GVec.Vector vec elem)
  => UnsafeSlice (GVec.Mutable vec (PrimState m) elem)
  -> UnsafeSlice (vec elem)
  -> m ()
safeCopy (UnsafeSlice to) (UnsafeSlice from) = GVec.copy to from

mutableMove
  :: (PrimMonad m, GMVec.MVector vec elem)
  => UnsafeSlice (vec (PrimState m) elem)
  -> UnsafeSlice (vec (PrimState m) elem)
  -> m ()
mutableMove (UnsafeSlice a) (UnsafeSlice b) = GMVec.move a b

-- | Creates a deep-copy of a vector with a cursor, in which elements before the cursor are aligned
-- at the start of the vector, and elements after the cursor are aligned at the end of the vector.
copyVec
  :: (GMVec.MVector vec elem, PrimMonad m)
  => vec (PrimState m) elem
  -> VecLength -> VecLength -> m (vec (PrimState m) elem)
copyVec oldVec (VecLength before) (VecLength after) = do
  let len = VecLength $ GMVec.length oldVec
  let (VecIndex upper) = indexAfterRange 0 $ len - VecLength after
  newVec <- GMVec.new (fromLength len)
  GMVec.copy (GMVec.unsafeSlice     0 before newVec) (GMVec.unsafeSlice     0 before oldVec)
  GMVec.copy (GMVec.unsafeSlice upper after  newVec) (GMVec.unsafeSlice upper after  oldVec)
  return newVec

----------------------------------------------------------------------------------------------------

-- Line indexing arithmetic
--
-- I think of these functions as being similar to named environment variables in that you can use
-- these names to have meaningful symbols for certain vector indicies. This makes code involving
-- ranges of lines, and code involving translating user-facing @('Absolute' 'LineIndex')@ values to
-- simplified arithmetic expressions that I consuder to be more human readable.


-- TODO:
--
-- Create 2 general buffer monads, one for mutable gap buffers, and one for immutable cursored
-- buffers. These monads should use a generic polymorphic vector type (so it should work the same
-- regardless of whether users choose boxed, unboxed, or storable type vectors), and a generic
-- polymorphic element type.
--
-- I thought about re-defining 'MonadEditVec' as any monad that contains the state of one of these
-- two general buffer monads, so that the only members of the 'MonadEditVec' typeclass would be the
-- lenses use to update the monadic state. __However, this would probably not be a good idea__
-- because 'MonadEditVec' as it is defined now can have both mutable and immutable vector states
-- instantiated into it, both types of which have a very different instantiation of 'MonadEditVec'.

class Monad m => MonadEditVec vec m | m -> vec where
  getAllocSize :: m VecLength
  modVector :: (vec -> vec) -> m vec
  -- | This is a value relative to the cursor, the integer value returned does not include the value
  -- under the cursor itself, as the value under the cursor may or may not exist.
  modCount  :: RelativeToCursor -> (VecLength -> VecLength) -> m VecLength
  -- | Isolate a sub-vector within the current vector using 'GMVec.slice' or 'GVec.slice'.
  sliceVector
    :: (vec -> UnsafeSlice vec)
    -> VecIndex -> VecLength
    -> m (UnsafeSlice vec)
  -- | Create a deep-copy of the @vec@. This function should just point to one of the various
  -- @clone@ functions in the @vector@ library. 
  cloneVector :: vec -> m vec
  -- | Create a new vector and copy each of the given slices to the new vector, one after the other.
  appendVectors :: vec -> vec -> m vec
  throwLimitErr :: RelativeToCursor -> m void
  -- | Returns a boolean indicating whether the element under the cursor is valid.
  getCursorIsDefined :: m Bool

class MonadEditVec vec m => MonadGapBuffer vec m | m -> vec where
  nullElem  :: vec ~ v elem => m elem
  newVector :: VecLength -> m vec
  setCursorIsDefined :: Bool -> m ()

-- not for export
--
-- Calls 'sliceVector' passing the private 'UnsafeSlice' constructor.
sliceVectorSafe :: MonadEditVec vec m => VecIndex -> VecLength -> m (UnsafeSlice vec)
sliceVectorSafe = sliceVector UnsafeSlice

-- | The vector of the text buffer.
getVector :: MonadEditVec vec m => m vec
getVector = modVector id

-- | When placing an element into the buffer, this function can returns the index in the array
-- 'Before' (or "under") the cursor, or the index 'After' the cursor, which are on either end of the
-- gap in the gap buffer.
indexNearCursor :: MonadEditVec vec m => RelativeToCursor -> m VecIndex
indexNearCursor = \ case
  Before -> cursorIndex
  After  -> indexAtRangeEnd 0 <$> (subtract <$> modCount After id <*> getAllocSize)

-- | The number of valid elements under the cursor, this number is either 0 or 1.
countOnCursor :: MonadEditVec vec m => m VecLength
countOnCursor = (\ a -> VecLength $ if a then 0 else 1) <$> getCursorIsDefined

-- | Calls 'modCount' but adds 'countOnCursor' if you are counting elements 'Before' the cursor.
countCursor :: MonadEditVec vec m => RelativeToCursor -> m VecLength
countCursor = \ case
  Before -> (+) <$> countOnCursor <*> modCount Before id
  After  -> modCount After id

-- | Returns the total number of defined elements.
countDefined :: MonadEditVec vec m => m VecLength
countDefined = (<>) <$> countCursor Before <*> countOnCursor

-- | The number of elements in the buffer that are not valid, @(bufAllocSize - bufLineCount)@
countUndefined :: MonadEditVec vec m => m VecLength
countUndefined = subtract <$> countDefined <*> getAllocSize

-- | The  cursor position,  regardless of whether  the element  under the cursor  is valid.  This is
-- equivalent to @('indexNearCursor' 'Before'@.
cursorIndex :: MonadEditVec vec m => m VecIndex
cursorIndex = indexAfterRange 0 <$> modCount Before id

-- | A predicate checking if the cursor is currently at the beginning or end of the buffer. This is
-- to be used as a check before evaluating 'popElem' to be sure 'popElem' will not evaluate to an
-- exception.
cursorAtEnd
  :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem)
  => RelativeToCursor -> m Bool
cursorAtEnd = fmap (0 ==) . flip modCount id

-- | Get the 'VecIndex' within the gap buffer that is associated with the given 'Absolute'
-- index. Bounds checking is __NOT__ performed.
absoluteIndex :: (Ord i, IsIndex i, MonadEditVec vec m) => i -> m VecIndex
absoluteIndex = fromIndex >>> \ i -> indexNearCursor Before >>= \ (VecIndex cur) ->
  let idx = VecIndex i in if i <= cur then pure idx else indexAfterRange idx <$> countUndefined

-- | Convert a 'VecIndex' to a logical 'Absolute' index. The returned 'Absolute' index value is
-- clamped to an in-bound value if the given value would otherwise be out of bounds.
indexToAbsolute :: (IsIndex i, Bounded i, MonadEditVec vec m) => VecIndex -> m i
indexToAbsolute i = fmap (\ (VecIndex i) -> toIndex i) $ countDefined >>= \ count ->
  let top = indexAtRangeEnd 0 count in
  if i >= top then return top
  else if i <= 0    then return 0
  else cursorIndex >>= \ cur ->
    if i <= cur then return i else indexAfterRange i <$> countUndefined

-- | Get the index within the vector that is associated with the given index value 'Relative' to the
-- cursor.
relativeIndex :: (Ord i, IsLength i, MonadEditVec vec m) => i -> m VecIndex
relativeIndex = (. unwrapRelative) $ \ i ->
  flip indexAfterRange i <$> indexNearCursor (if i <= 0 then Before else After)

-- | Given an index value, return a 'VecLength' indicating the distance the given index is from the
-- gap buffer cursor.
distanceFromCursor :: MonadEditVec vec m => VecIndex -> m VecLength
distanceFromCursor i = indexDistance i <$> cursorIndex

-- | This function computes the normalized, physical 'VecIndex' for any logical index value @i@ as
-- long as @i@ instantiates the 'IsIndex'. The type @i@ is usually @('Absolute' 'LineIndex')@ when
-- @m@ is 'Hakshell.TextEditor.TextEdit' or @('Absolute' 'CharIndex')@ when @m@ is
-- 'Hakshell.TextEditor.LineEdit'. A "normalized" 'VecIndex' value means 'minBound' is clamped to
-- zero, 'maxBound' is clamped to the top of the state 'GVec.Vector' of @m@.
--
-- This function returns a pair of values. The first value returned is a 'Bool' indicating whether
-- the 'VecIndex' value computed from @i@ is in-bounds with respect to the state 'GVec.Vector' of
-- @m@ ('False' means it is invalid). The second value returned the normalized 'VecIndex' value
-- which has been clamped to the nearest in-bounds value for the state 'GVec.Vector' of @m@.
--
-- You can ignore the returned 'Bool' value and use this function to simply force values of @i@ to
-- be in-bounds. You can use the 'validateIndex' function if you want a function which throws an
-- catchable exception when the value @i@ is out-of-bounds.
testIndex
  :: (Ord i, Bounded i, IsIndex i,
      Monad m, MonadEditVec vec m, MonadError BufferError m
     )
  => i -> m (Bool, VecIndex)
testIndex i = countDefined >>= \ count ->
  if count == 0         then throwError BufferIsEmpty
  else if i == minBound then return (True, 0)
  else if i == maxBound then return (True, indexAtRangeEnd 0 count)
  else absoluteIndex i >>= \ i -> return $ let top = indexAtRangeEnd 0 count in
    if i > top then (False, top) else if i < 0 then (False, 0) else (True , i)

-- | This function inspects the 'Bool' value of the pair returned by 'testIndex'. If the 'Bool'
-- value is 'False', an exception is thrown, otherwise the normalized 'VecIndex' is returned.
validateIndex
  :: (Ord i, Bounded i, IsIndex i,
      Monad m, MonadEditVec vec m, MonadError BufferError m
     )
  => i -> m VecIndex
validateIndex = testIndex >=> \ (ok, i) ->
  if ok then return i else throwError (BufferIndexBounds i)

-- | This function is similar to 'validateIndex', but checks the given relative index value, which
-- is used to compute an absolute index that is checked with 'testIndex', which must be valid in
-- order for the given relative index value to be valid. This function throws an exception if the
  -- relative index is invalid, and returns a valid 'VecLength'.
validateLength
  :: (Ord rel, IsLength rel,
      Monad m, MonadError BufferError m,
      MonadGapBuffer vec m
     )
  => rel -> m VecLength
validateLength rel0 = do
  let rel = unwrapRelative rel0
  (ok, _) <- relativeIndex rel0 >>= testIndex
  unless ok $ do
    i <- cursorIndex
    throwError $ BufferIndexRange i $ unwrapRelative rel
  return rel

-- | Return the starting and ending index of the void region of the buffer, which is the contiguous
-- region of undefined elements within the buffer.
getVoid :: MonadEditVec vec m => m (Maybe (VecIndex, VecIndex))
getVoid = do
  rgn <- (,) <$> ((+ 1) <$> indexNearCursor Before) <*> (subtract 1 <$> indexNearCursor After)
  return $ guard (uncurry (<=) rgn) >> Just rgn

-- | Make a slice of elements relative to the current cursor. A negative argument will take elements
-- before and up-to the cursor, a positive argument will take that many elements starting from
-- 'getLineAfterCur'. Pass a boolean value indicating whether or not you would like to perform
-- bounds checking, if so an exception will be raised if the line index goes out of bounds.
sliceFromCursor :: MonadEditVec vec m => VecLength -> m (UnsafeSlice vec)
sliceFromCursor count =
  if count < 0 then indexNearCursor Before >>=
    flip sliceVectorSafe (abs count) . flip indexAfterRange count
  else if count > 0 then indexNearCursor After >>= flip sliceVectorSafe count
  else sliceVectorSafe 0 0

-- | Create a slice between two indicies, creating a clone array with the content of the slice. The
-- slice is never empty because if both indicies are the same, a vector containing that single
-- element at that index is returned.
sliceBetween :: MonadEditVec vec m => VecIndex -> VecIndex -> m vec
sliceBetween from = sliceVectorSafe from . indexRange from >=>
  cloneVector . unwrapUnsafeSlice

-- | Like 'sliceBetween' but does not create a clone of the vector, so the returned vector is unsafe
-- and must be safely evaluated.
safeSliceBetween
  :: MonadEditVec vec m
  => VecIndex -> VecIndex
  -> (UnsafeSlice vec -> m a)
  -> m a
safeSliceBetween from to = ((sliceVectorSafe from $ indexRange from to) >>=)

-- | Make a slice of the void region of the buffer (the contiguous region of invalid elements after
-- the cursor). This can be used as the target of a vector copy. If the buffer is full, 'Nothing' is
-- returned.
withVoidSlice
  :: (GMVec.MVector vec elem, MonadEditVec (vec (PrimState m) elem) m)
  => VecLength
  -> (UnsafeSlice (vec (PrimState m) elem) -> m ())
  -> m Bool
withVoidSlice count f = getVoid >>= maybe (pure False) cont where
  cont (lo, hi) = do
    if count >= 0 then sliceVectorSafe lo count >>= f else
      sliceVectorSafe (indexAfterRange hi count) (negate count) >>= f
    return True

getSlice :: MonadEditVec vec m => RelativeToCursor -> m (UnsafeSlice vec)
getSlice = \ case { Before -> getLoSlice; After -> getHiSlice; }

-- | Obtain a slice (using 'sliceFromCursor') for the portion of the vector containing elements
-- before or on the current cursor.
getLoSlice :: MonadEditVec vec m => m (UnsafeSlice vec)
getLoSlice = countCursor Before >>= sliceFromCursor . negate

-- | Obtain a slice (using 'sliceFromCursorM') for the portion of the vector containing elements
-- after the current cursor.
getHiSlice :: MonadEditVec vec m => m (UnsafeSlice vec)
getHiSlice = countCursor After >>= sliceFromCursor

-- | Given a region, slice the vector to the region and evaluate one of two given continuations with
-- the slice. If the region spans the gap in the buffer, the first continuation is called. If the
-- region is fully contained to within either sub region on either side of the gap, then the second
-- continuation function is called.
withRegion
  :: MonadEditVec vec m
  => VecIndex -> VecIndex
  -> (UnsafeSlice vec -> UnsafeSlice vec -> m a)
  -> m a
withRegion from0 to0 f =
  cursorIndex >>= \ cur ->
  absoluteIndex (min from0 to0) >>= \ from ->
  absoluteIndex (max from0 to0) >>= \ to ->
  let contained =  safeSliceBetween from to in
  if      from <= cur && to <= cur then contained $ safeSliceBetween 0 0 . f
  else if from >  cur && to >  cur then contained $ safeSliceBetween 0 0 . flip f
  else do
    bef <- indexNearCursor Before
    aft <- indexNearCursor After
    safeSliceBetween from bef $ safeSliceBetween aft to . f

-- | Similar to 'withRegion' but operates on the entire defined region of the buffer.
withFullSlices
  :: MonadEditVec vec m
  => (UnsafeSlice vec -> UnsafeSlice vec -> m a)
  -> m a
withFullSlices f = join $ f <$> getLoSlice <*> getHiSlice

-- | Create a new vector, copy the portion of the state tracking the old vector and the cursor
-- positions onto the stack, then evaluate a continuation function of type @m@ with the new vector
-- in place. Note that the cursor is not modified when evaluation of the continuation begins, even
-- if the cursor would be out of bounds. Once the continuation function completes, restore the prior
-- state and return the new vector.
withNewVector
  :: (PrimMonad m, GMVec.MVector vec elem, MonadGapBuffer (vec (PrimState m) elem) m)
  => VecLength -> m () -> m (vec (PrimState m) elem)
withNewVector size f = do
  oldvec <- modVector id
  before <- modCount Before id
  after  <- modCount After  id
  newvec <- newVector size
  modVector (const newvec)
  () <- f
  modCount Before (const before)
  modCount After  (const after )
  modVector (const oldvec)
  return newvec

-- | Select a region of valid elements given an index and size values, taking into account the
-- position of the cursor, and copy the region into a new contiguous mutable vector that contains no
-- invalid elements. It is usually expected that the mutable vector will be frozen by the function
-- which called 'copyRegion'.
copyRegion
  :: (PrimMonad m, GMVec.MVector vec elem, MonadGapBuffer (vec (PrimState m) elem) m)
  => VecIndex -> VecIndex -> m (vec (PrimState m) elem)
copyRegion from to = withRegion from to $ \ oldLo oldHi -> do
  withNewVector (sliceSize GMVec.length oldLo + sliceSize GMVec.length oldHi) $ do
    modCount Before $ const $ sliceSize GMVec.length oldLo
    modCount After  $ const $ sliceSize GMVec.length oldHi
    withFullSlices $ \ newLo newHi -> do
      mutableCopy newLo oldLo
      mutableCopy newHi oldHi

-- | Write an element to a vector index, overwriting whatever was there before. __WARNING__: there
-- is no bounds checking.
putElemIndex
  :: (PrimMonad m, GMVec.MVector vec elem, MonadEditVec (vec (PrimState m) elem) m)
  => VecIndex -> elem -> m ()
putElemIndex i elem = join $ asrtMWrite UnsafeOp "putElemIndex"
  <$> ((,) "getVector" <$> getVector)
  <*> pure (asrtShow "i" i)
  <*> pure elem

-- | Read an element from a vector index. __WARNING__: there is no bounds checking.
getElemIndex
  :: (PrimMonad m, GMVec.MVector vec elem, MonadEditVec (vec (PrimState m) elem) m)
  => VecIndex -> m elem
getElemIndex i = join $ asrtMRead UnsafeOp "getElemIndex"
  <$> ((,) "getVector" <$> getVector)
  <*> pure (asrtShow "i" i)

-- | Put an element at the cursor. This function evaluates (modCursorIsDefined $ const True).
putElem
  :: (PrimMonad m, GMVec.MVector vec elem, MonadGapBuffer (vec (PrimState m) elem) m)
  => RelativeToCursor -> elem -> m ()
putElem rel elem = do
  indexNearCursor rel >>= flip putElemIndex elem
  setCursorIsDefined True

-- | Get an element from the cursor. If the element is not defined ((modCursorIsDefined id) returns
-- false), an exception is thrown.
getElem
  :: (PrimMonad m, GMVec.MVector vec elem, MonadGapBuffer (vec (PrimState m) elem) m)
  => RelativeToCursor -> m elem
getElem rel = do
  defined <- getCursorIsDefined
  if defined then indexNearCursor rel >>= getElemIndex else throwLimitErr Before

-- | Sets the element under the cursor to an undefined value, but does not move the cursor.
delElem
  :: (PrimMonad m, GMVec.MVector vec elem, MonadGapBuffer (vec (PrimState m) elem) m)
  => RelativeToCursor -> m ()
delElem rel = do
  join $ putElemIndex <$> indexNearCursor rel <*> nullElem
  setCursorIsDefined False

-- | Takes two paramters @oldSize@ and @newSize@. This function finds the minimum power of two
-- scalara necessary to scale the @oldSize@ such that it is __greater_than__ the @newSize@. Said
-- another way, this function repeatedly multiplies @oldSize@ by 2 until it is greater than (not
-- equal to) the @newSize@ value.
--
-- Note that this function does ever not return a value equal to @newSize@, the assumption is that
-- this function is used to grow a buffer, not to make it exactly the size required. If you want
-- this function to return a value equal to the exact amount, try passing a @newSize@ value equal to
-- @oldSize * 2^n - 1@ for some integer @n@.
minPow2ScaledSize :: VecLength -> VecLength -> VecLength
minPow2ScaledSize oldsiz newsiz = head $ dropWhile (<= newsiz) $ iterate (* 2) $ max 1 oldsiz

-- | Pass a minimum vector allocation size request as an argument. This function check the
-- allocation size (using 'getAllocSize') of the current buffer vector. If the current allocation is
-- larger than the size request argument, 'Nothing' is returned. If the current allocation is
-- smaller than the size request, a new vector is allocated and returned in a 'Just'
-- constructor. The new buffer is ONLY allocated and returned; the content of the current buffer is
-- NOT copied, the current buffer is not replaced.
prepLargerVector
  :: (GMVec.MVector vec elem, MonadGapBuffer (vec (PrimState m) elem) m)
  => VecLength -> m (Maybe (vec (PrimState m) elem))
prepLargerVector newsiz = do
  oldsiz <- getAllocSize
  if oldsiz >= newsiz then return Nothing else
    Just <$> newVector (minPow2ScaledSize oldsiz newsiz)

-- | If and only if the vector is full, allocate a new vector with enough space to fit the requested
-- number of elements by doubling the size of the current vector until the size is large enough,
-- then copy the elements from the current vector to the new vector, and then replace the current
-- vector with the new one.
growVector
  :: (PrimMonad m, GMVec.MVector vec elem, MonadGapBuffer (vec (PrimState m) elem) m)
  => VecLength -> m ()
growVector increase = if increase <= 0 then return () else
  countDefined >>= \ count -> prepLargerVector (count <> increase) >>= \ case
    Nothing     -> return ()
    Just newvec -> withFullSlices $ \ oldbef oldaft -> do
      modVector $ const newvec
      withFullSlices $ \ newbef newaft ->
        mutableCopy newbef oldbef >> mutableCopy newaft oldaft

-- | Push a single element to the index 'Before' (currently on) the cursor, or the index 'After' the
-- cursor, and then shift the cursor to point to the pushed element.
pushElem
  :: (PrimMonad m, GMVec.MVector vec elem, MonadGapBuffer (vec (PrimState m) elem) m)
  => RelativeToCursor -> elem -> m ()
pushElem rel elem = growVector 1 >> modCount rel (+ 1) >>
  indexNearCursor rel >>= flip putElemIndex elem

-- | Push a vector of elements starting at the index 'Before' (currently on) the cursor, or the
-- index 'After' the cursor.
pushElemVec
  :: (PrimMonad m, GVec.Vector vec elem,
      mvec (PrimState m) elem ~ GVec.Mutable vec (PrimState m) elem,
      MonadGapBuffer (mvec (PrimState m) elem) m
     )
  => RelativeToCursor -> vec elem -> m ()
pushElemVec rel src = do
  let len = VecLength $ (case rel of{ Before -> negate; After -> id; }) $ GVec.length src
  growVector len
  ok <- withVoidSlice len $ \ (UnsafeSlice targ) -> modCount rel (+ len) >> GVec.copy targ src
  unless ok $ error $ "pushElemVec: internal error, \"growVector\" failed to create space"

-- | Pop a single element from the index 'Before' (currently on) the cursor, or from the index 'After'
-- the cursor, and then shift the cursor to point to the pushed element.
popElem
  :: (PrimMonad m, GMVec.MVector vec elem,
      MonadError BufferError m,
      MonadGapBuffer (vec (PrimState m) elem) m)
  => RelativeToCursor -> m elem
popElem rel = cursorAtEnd rel >>= flip when (throwError $ BufferLimitError rel) >>
  getElem rel <* delElem rel <* modCount rel (subtract 1)

-- | Move the cursor, which will shift elements around the vector, using a algorithm of O(n) steps,
-- where @n@ is the 'Relative' shift value. Pass a boolean value indicating whether or not you would
-- like to perform bounds checking, if so an exception will be raised if the line index goes out of
-- bounds.
shiftCursor
  :: (PrimMonad m, GMVec.MVector vec elem,
      MonadGapBuffer (vec (PrimState m) elem) m,
      MonadError BufferError m
     )
  => VecLength -> m ()
shiftCursor count0 =
  -- TODO: rename this function to 'mutableShiftCursor', add 'shiftCursor' to the 'MonadEditVec'
  -- typeclass, use 'mutableShiftCursor' to instantiate 'shiftCursor' for mutable vector editing
  -- monads, and use the 'viewerCursorTo' function to instantiate 'shiftCursor' for the immutable
  -- vector editing monads.
  if count == 0 then return () else getVoid >>= \ case
    Nothing -> done
    Just ~(lo, hi) ->
      if      count ==  1 then popElem After  >>= pushElem Before
      else if count == -1 then popElem Before >>= pushElem After
      else do
        from <- sliceFromCursor count
        to   <-
          if      count >  1 then sliceVectorSafe lo count
          else if count < -1 then sliceVectorSafe (indexAfterRange (hi + 1) count) (negate count)
          else error "shiftCursor: internal error, this should never happen"
        mutableMove to from
        done
    where
      count  = unwrapRelative count0
      done   = do
        before <- modCount Before (+ count)
        modCount After (subtract count)
        when (before == 0) $ setCursorIsDefined False

moveCursorBy
  :: (Ord i, Bounded i, IsIndex i,
      Ord rel, IsLength rel,
      GMVec.MVector vec elem,
      Monad m, PrimMonad m,
      MonadError BufferError m,
      MonadEditVec (vec (PrimState m) elem) m,
      MonadGapBuffer (vec (PrimState m) elem) m
     )
  => rel -> m i
moveCursorBy = validateLength >=> shiftCursor >=> const (cursorIndex >>= indexToAbsolute)

moveCursorTo
  :: (Ord i, Bounded i, IsIndex i,
      PrimMonad m, GMVec.MVector vec elem,
      MonadGapBuffer (vec (PrimState m) elem) m,
      MonadError BufferError m
     )
  => i -> m i
moveCursorTo =
  validateIndex >=> distanceFromCursor >=> shiftCursor >=>
  const (cursorIndex >>= indexToAbsolute)

-- | Copy the current mutable buffer vector to an immutable vector.
freezeVector
  :: (PrimMonad m, GMVec.MVector mvec elem, GVec.Vector vec elem,
      mvec (PrimState m) elem ~ GVec.Mutable vec (PrimState m) elem,
      MonadGapBuffer (mvec (PrimState m) elem) m)
  => m (vec elem)
freezeVector = do
  oldvec <- getVector
  newvec <- countDefined >>= newVector
  withFullSlices $ \ oldLo oldHi -> do
    modVector (const newvec)
    withFullSlices $ \ newLo newHi -> mutableCopy newLo oldLo >> mutableCopy newHi oldHi
    modVector (const oldvec)
    safeFreeze (UnsafeSlice newvec)

----------------------------------------------------------------------------------------------------

data BufferError
  = BufferIsEmpty
  | UndefinedElement  VecIndex
  | BufferLimitError  RelativeToCursor
  | BufferIndexBounds VecIndex
  | BufferIndexRange  VecIndex VecLength
  deriving (Eq, Ord, Show)

----------------------------------------------------------------------------------------------------

data GapBufferState vec
  = GapBufferState
    { theGapBufferVector          :: !vec
    , theGapBufferCursorIsDefined :: !Bool
    , theGapBufferBeforeCursor    :: !VecLength
    , theGapBufferAfterCursor     :: !VecLength
    }

gapBufferVector :: Lens' (GapBufferState vec) vec
gapBufferVector = lens theGapBufferVector $ \ a b -> a{ theGapBufferVector = b }

gapBufferCursorIsDefined :: Lens' (GapBufferState vec) Bool
gapBufferCursorIsDefined = lens theGapBufferCursorIsDefined $ \ a b ->
  a{ theGapBufferCursorIsDefined = b }

gapBufferBeforeCursor :: Lens' (GapBufferState vec) VecLength
gapBufferBeforeCursor = lens theGapBufferBeforeCursor $ \ a b -> a{ theGapBufferBeforeCursor = b }

gapBufferAfterCursor :: Lens' (GapBufferState vec) VecLength
gapBufferAfterCursor = lens theGapBufferAfterCursor $ \ a b -> a{ theGapBufferAfterCursor = b }

gapBufferLength :: GMVec.MVector vec elem => GapBufferState (vec st elem) -> VecLength
gapBufferLength = VecLength . GMVec.length . theGapBufferVector

----------------------------------------------------------------------------------------------------

newtype GapBuffer vec m a
  = GapBuffer
    { unwrapGapBuffer ::
        ExceptT BufferError (StateT (GapBufferState vec) m) a
    }
  deriving
    ( Functor, Applicative, Monad, MonadIO,
      MonadState (GapBufferState vec),
      MonadError BufferError
    )

--instance Monad m => MonadError (BufferError vec) (GapBuffer vec m) where
--  throwError = GapBuffer . throwError
--  catchError (GapBuffer try) catch = GapBuffer $ catchError try $ unwrapGapBuffer . catch
--
--instance Monad m => MonadState (GapBufferState vec) (GapBuffer vec m) where
--  state = GapBuffer . state

instance MonadTrans (GapBuffer vec) where { lift = GapBuffer . lift . lift; }

instance PrimMonad m => PrimMonad (GapBuffer vec m) where
  type PrimState (GapBuffer vec m) = PrimState m
  primitive = lift . primitive

instance (PrimMonad m, GMVec.MVector vec elem, st ~ PrimState m) =>
  MonadEditVec (vec st elem) (GapBuffer (vec st elem) m)
  where
    getAllocSize = VecLength . GMVec.length <$> use gapBufferVector
    modVector    = modifyAndUse gapBufferVector
    modCount     = \ case
      Before -> modifyAndUse gapBufferBeforeCursor
      After  -> modifyAndUse gapBufferAfterCursor
    cloneVector  = lift . GMVec.clone
    sliceVector constr (VecIndex from) (VecLength size) =
      constr . GMVec.unsafeSlice from size <$> use gapBufferVector
    appendVectors a b = do
      let sizeA   = GMVec.length a
      let sizeB   = GMVec.length b
      let sizeNew = sizeA + sizeB
      newvec <- GMVec.new sizeNew
      GMVec.copy (GMVec.unsafeSlice     0 sizeA newvec) a
      GMVec.copy (GMVec.unsafeSlice sizeA sizeB newvec) b
      return newvec
    throwLimitErr = throwError . BufferLimitError
    getCursorIsDefined = use gapBufferCursorIsDefined

-- | Evaluate a 'GapBuffer' function providing it a new vector with which to instantiate the
-- 'GapBufferState'.
runGapBufferNew
  :: vec -> GapBuffer vec m a
  -> m (Either BufferError a, GapBufferState vec)
runGapBufferNew vec (GapBuffer (ExceptT f)) = runStateT f GapBufferState
  { theGapBufferVector          = vec
  , theGapBufferCursorIsDefined = False
  , theGapBufferBeforeCursor    = 0
  , theGapBufferAfterCursor     = 0
  }

-- | Evaluate a 'GapBuffer' using a 'GapBufferState' value from a prior run of a 'GapBuffer'
-- function.
runGapBuffer
  :: GapBuffer vec m a
  -> GapBufferState vec
  -> m (Either BufferError a, GapBufferState vec)
runGapBuffer (GapBuffer (ExceptT f)) = runStateT f

cloneGapBufferState
  :: (GMVec.MVector vec elem, PrimMonad m)
  => GapBuffer (vec (PrimState m) elem) m (GapBufferState (vec (PrimState m) elem))
cloneGapBufferState = do
  gbst   <- get
  newVec <- copyVec
    (theGapBufferVector gbst)
    (theGapBufferBeforeCursor gbst)
    (theGapBufferAfterCursor gbst)
  return $ gbst{ theGapBufferVector = newVec }

----------------------------------------------------------------------------------------------------

-- | The buffer within is immutable and there is no gap, and it is indexed with a cursor. The "II"
-- in "IIBuffer" means "indexed immutable."
data IIBufferState vec
  = IIBufferState
    { theIIBufferVector :: !vec
    , theIIBufferCursor :: !VecLength
    }

iiBufferVector :: Lens' (IIBufferState vec) vec
iiBufferVector = lens theIIBufferVector $ \ a b -> a{ theIIBufferVector = b }

iiBufferCursor :: Lens' (IIBufferState vec) VecLength
iiBufferCursor = lens theIIBufferCursor $ \ a b -> a{ theIIBufferCursor = b }

----------------------------------------------------------------------------------------------------

newtype IIBuffer vec m a
  = IIBuffer{ unwrapIIBuffer :: ExceptT BufferError (StateT (IIBufferState vec) m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => MonadError BufferError (IIBuffer vec m) where
  throwError = IIBuffer . throwError
  catchError (IIBuffer try) catch = IIBuffer $ catchError try $ unwrapIIBuffer . catch

instance Monad m => MonadState (IIBufferState vec) (IIBuffer vec m) where
  state = IIBuffer . state

instance MonadTrans (IIBuffer vec) where { lift = IIBuffer . lift . lift; }

instance PrimMonad m => PrimMonad (IIBuffer vec m) where
  type PrimState (IIBuffer vec m) = PrimState m
  primitive = lift . primitive

instance (Monad m, GVec.Vector vec elem) => MonadEditVec (vec elem) (IIBuffer (vec elem) m) where
  getAllocSize = VecLength . GVec.length <$> use iiBufferVector
  modVector f = iiBufferVector %= f >> use iiBufferVector
  modCount = \ case
    Before -> \ f -> iiBufferCursor %= f >> use iiBufferCursor
    After  -> \ f -> getAllocSize >>= \ siz ->
      iiBufferCursor %= (siz -) . f . (siz -) >> use iiBufferCursor
  sliceVector constr (VecIndex i) (VecLength len) =
    constr . GVec.unsafeSlice i len <$> use iiBufferVector
  cloneVector = pure
  appendVectors a b = pure $ a GVec.++ b
  throwLimitErr = throwError . BufferLimitError
  getCursorIsDefined = (/= 0) <$> getAllocSize

runIIBuffer
  :: Monad m
  => IIBuffer vec m a
  -> IIBufferState vec
  -> m (Either BufferError a, IIBufferState vec)
runIIBuffer (IIBuffer (ExceptT f)) = runStateT f

gapBufferLiftIIBuffer
  :: Monad m
  => IIBuffer vec m a
  -> IIBufferState vec
  -> GapBuffer mvec m (a, IIBufferState vec)
gapBufferLiftIIBuffer f st = lift (runIIBuffer f st) >>= \ (a, st) ->
  flip (,) st <$> (throwError ||| pure) a
