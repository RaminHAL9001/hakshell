-- | This module implements a generic, mutable vector with a cursor. Moving the cursor shifts
-- elements after the cursor toward the end of the vector, ensuring pushing elements is always an
-- O(1) operation as long as there is space available. Moving the cursor is an O(n) operation, where
-- /n/ is the distance of the cursor movement.
module Hakshell.GapBuffer
  ( IsIndex(..), IsLength(..), HasOpposite(..),
    VecIndex(..), VecLength(..), indexAfterRange, indexAtRangeEnd,
    Relative(..), RelativeToCursor(..), relative, unwrapRelative,
    Absolute(..), absoluteIndex, indexToAbsolute,
    MonadEditVec(..), MonadGapBuffer(..), getVector,
    indexNearCursor, cursorIndex, cursorAtEnd, relativeIndex, guardVecIndex,
    countOnCursor, countDefined, countUndefined,
    getVoid, sliceFromCursor, sliceBetween, safeSliceBetween, withVoidSlice, getSlice,
    UnsafeSlice, SafeEvalSlice, SafeSliceEvaluator,
    withFullSlices, withRegion, withNewVector, copyRegion,
    putElemIndex, getElemIndex, putElem, getElem, delElem,
    pushElem, pushElemVec, popElem, shiftCursor,
    minPow2ScaledSize, prepLargerVector, growVector, freezeVector, copyVec,
  ) where

import           Control.Arrow
import           Control.Monad.Except
import           Control.Monad.Primitive
import           Control.Monad.ST

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

-- | Distance between two 'VecIndex' values. This function always returns a positive (non-zero)
-- number because it is designed to produce a 'VecLength' value that selects all elements between
-- two delimiting indicies, including the elements at the delimited indicies.
indexDistance :: VecIndex -> VecIndex -> VecLength
indexDistance (VecIndex a) (VecIndex b) = VecLength $ 1 + abs (subtract a b)

----------------------------------------------------------------------------------------------------

-- | This is an opaque wrapper which is used to wrap unsafe copies of vectors, these are duplicates
-- of a pointer into the vector, but not a deep-copy of the elements. This is unsafe because it can
-- lead to null-pointer exceptions. To make it safer, we make use of Haskell's type system to
-- restrict the functions in which it can be used.
newtype UnsafeSlice vec = UnsafeSlice { unwrapUnsafeSlice :: vec }

-- | This is a simple monadic context in which an 'UnsafeSlice' can be manipulated safely. Some APIs
-- in this module take a continuation of this function type and evalaute it, ensuring evaluation
-- exists within the limits of the calling context to which the continuation was passed.
newtype SafeEvalSlice m a = SafeEvalSlice{ evalSliceSafely :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans SafeEvalSlice where { lift = SafeEvalSlice; }

-- | A function of this type is usually passed to a 'SafeEvalSlice' continuation function. This
-- allows you to unwrap other 'SafeEvalSlice' functions, but only within the context of the
-- continuation so that safety is not lost.
type SafeSliceEvaluator m a = SafeEvalSlice m a -> m a

sliceSize :: (vec -> Int) -> UnsafeSlice vec -> VecLength
sliceSize length (UnsafeSlice vec) = VecLength $ length vec

safeFreeze
  :: (MonadIO m, GVec.Vector vec elem)
  => UnsafeSlice (GVec.Mutable vec RealWorld elem) -> SafeEvalSlice m (vec elem)
safeFreeze (UnsafeSlice vec) = liftIO (GVec.freeze vec)

mutableClone
  :: (MonadIO m, GMVec.MVector vec elem)
  => UnsafeSlice (vec RealWorld elem)
  -> SafeEvalSlice m (vec RealWorld elem)
mutableClone (UnsafeSlice vec) = liftIO (GMVec.clone vec)

clone :: GVec.Vector vec elem => UnsafeSlice (vec elem) -> vec elem
clone (UnsafeSlice vec) = runST (GNew.run (GVec.clone vec) >>= GVec.freeze)

copy
  :: (MonadIO m, GVec.Vector vec elem)
  => UnsafeSlice (GVec.Mutable vec RealWorld elem)
  -> UnsafeSlice (vec elem)
  -> SafeEvalSlice m ()
copy (UnsafeSlice mvec) (UnsafeSlice vec) = liftIO (GVec.copy mvec vec)

-- | The expression @('mutableCopy' a b)@ copies the elements from @b@ into the elements of @a@,
-- overwriting what was in @a@ before, and leaving @b@ unchanged.
mutableCopy
  :: (MonadIO m, GMVec.MVector vec elem)
  => UnsafeSlice (vec RealWorld elem)
  -> UnsafeSlice (vec RealWorld elem)
  -> SafeEvalSlice m ()
mutableCopy (UnsafeSlice a) (UnsafeSlice b) = liftIO (GMVec.copy a b)

mutableMove
  :: (MonadIO m, GVec.Vector vec elem)
  => UnsafeSlice (GVec.Mutable vec RealWorld elem)
  -> UnsafeSlice (GVec.Mutable vec RealWorld elem)
  -> SafeEvalSlice m ()
mutableMove (UnsafeSlice a) (UnsafeSlice b) = liftIO (GMVec.move a b)

foldVec
  :: (GVec.Vector vec elem, Monad m)
  => (fold -> elem -> m fold) -> fold -> UnsafeSlice (vec elem)
  -> SafeEvalSlice m fold
foldVec f st (UnsafeSlice vec) = lift (GVec.foldM' f st vec)

--foldMutable
--  :: MonadIO m
--  => (fold -> elem -> m fold) -> fold -> UnsafeSlice (GVec.Mutable vec RealWorld elem)
--  -> SafeEvalSlice m fold
--foldMutable f st (UnsafeSlice vec) = 

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
  -- | Isolate a sub-vector within the current vector using 'GMVec.slice' or 'GVec.slice'. You will
  -- have to 'lift' the result to construct the 'SafeEvalSlice' function type.
  sliceVector
    :: (vec -> UnsafeSlice vec)
    -> VecIndex -> VecLength
    -> SafeEvalSlice m (UnsafeSlice vec)
  -- | Create a deep-copy of the @vec@. This function should just point to one of the various
  -- @clone@ functions in the @vector@ library. 
  cloneVector :: vec -> m vec
  -- | Create a new vector and copy each of the given slices to the new vector, one after the other.
  appendVectors :: vec -> vec -> m vec
  -- | Returns a boolean indicating whether the element under the cursor is valid.
  throwLimitErr :: RelativeToCursor -> m void
  -- | Returns 'True' if the cursor is on a defined element. For mutable vectors, this can be false
  -- if the vector is empty. For immutable vectors, this can be false if the cursor index value is
  -- equivalent to the vector length value.
  getCursorIsDefined :: m Bool

class MonadEditVec vec m => MonadGapBuffer vec m | m -> vec where
  nullElem  :: vec ~ v elem => m elem
  newVector :: VecLength -> m vec
  setCursorIsDefined :: Bool -> m ()

-- not for export
--
-- Calls 'sliceVector' passing the private 'UnsafeSlice' constructor.
sliceVectorSafe :: MonadEditVec vec m => VecIndex -> VecLength -> SafeEvalSlice m (UnsafeSlice vec)
sliceVectorSafe = sliceVector UnsafeSlice

-- | The vector of the text buffer.
getVector :: MonadEditVec vec m => m vec
getVector = modVector id

-- | When placing an element into the buffer, this function can returns the index in the array
-- 'Before' (or "under") the cursor, or the index 'After' the cursor, which are on either end of the
-- gap in the gap buffer.
indexNearCursor :: MonadEditVec vec m => RelativeToCursor -> m VecIndex
indexNearCursor = \ case
  Before -> indexAfterRange 0 <$> modCount Before id
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

-- | The cursor position, regardless of whether the element under the cursor is valid.
cursorIndex :: MonadEditVec vec m => m VecIndex
cursorIndex = indexAfterRange 0 <$> modCount Before id

-- | A predicate checking if the cursor is currently at the beginning or end of the buffer. This is
-- to be used as a check before evaluating 'popElem' to be sure 'popElem' will not evaluate to an
-- exception.
cursorAtEnd
  :: (Semigroup (m (Relative Int)), MonadEditVec (vec st elem) m, GMVec.MVector vec elem)
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

-- | Throw an error if the 'VecIndex' is out of bounds.
guardVecIndex :: (MonadError err m, MonadEditVec vec m) => err -> VecIndex -> m VecIndex
guardVecIndex err i = if i < 0 then throwError err else getAllocSize >>= \ siz ->
  if i >= indexAfterRange 0 siz then throwError err else return i

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
sliceFromCursor :: MonadEditVec vec m => VecLength -> SafeEvalSlice m (UnsafeSlice vec)
sliceFromCursor count =
  if count < 0 then lift (indexNearCursor Before) >>=
    flip sliceVectorSafe (abs count) . flip indexAfterRange count
  else if count > 0 then lift (indexNearCursor After) >>= flip sliceVectorSafe count
  else sliceVectorSafe 0 0

-- | Create a slice between two indicies, creating a clone array with the content of the slice. The
-- slice is never empty because if both indicies are the same, a vector containing that single
-- element at that index is returned.
sliceBetween :: MonadEditVec vec m => VecIndex -> VecIndex -> m vec
sliceBetween from = evalSliceSafely . sliceVectorSafe from . indexDistance from >=>
  cloneVector . unwrapUnsafeSlice

-- | Like 'sliceBetween' but does not create a clone of the vector, so the returned vector is unsafe
-- and must be safely evaluated.
safeSliceBetween
  :: MonadEditVec vec m
  => VecIndex -> VecIndex
  -> (UnsafeSlice vec -> SafeEvalSlice m a)
  -> m a
safeSliceBetween from to = evalSliceSafely . ((sliceVectorSafe from $ indexDistance from to) >>=)

-- | Make a slice of the void region of the buffer (the contiguous region of invalid elements after
-- the cursor). This can be used as the target of a vector copy. If the buffer is full, 'Nothing' is
-- returned.
withVoidSlice
  :: MonadEditVec vec m
  => VecLength
  -> (UnsafeSlice vec -> SafeEvalSlice m ())
  -> m Bool
withVoidSlice count f = getVoid >>= maybe (pure False) cont where
  cont (lo, hi) = evalSliceSafely $ do
    if count >= 0 then sliceVectorSafe lo count >>= f else
      sliceVectorSafe (indexAfterRange hi count) (negate count) >>= f
    return True

getSlice :: MonadEditVec vec m => RelativeToCursor -> SafeEvalSlice m (UnsafeSlice vec)
getSlice = \ case { Before -> getLoSlice; After -> getHiSlice; }

-- | Obtain a slice (using 'sliceFromCursor') for the portion of the vector containing elements
-- before or on the current cursor.
getLoSlice :: MonadEditVec vec m => SafeEvalSlice m (UnsafeSlice vec)
getLoSlice = lift (countCursor Before) >>= sliceFromCursor . negate

-- | Obtain a slice (using 'sliceFromCursorM') for the portion of the vector containing elements
-- after the current cursor.
getHiSlice :: MonadEditVec vec m => SafeEvalSlice m (UnsafeSlice vec)
getHiSlice = lift (countCursor After) >>= sliceFromCursor

-- | Given a region, slice the vector to the region and evaluate one of two given continuations with
-- the slice. If the region spans the gap in the buffer, the first continuation is called. If the
-- region is fully contained to within either sub region on either side of the gap, then the second
-- continuation function is called.
withRegion
  :: MonadEditVec vec m
  => VecIndex -> VecIndex
  -> (UnsafeSlice vec -> UnsafeSlice vec -> SafeEvalSlice m a)
  -> m a
withRegion from0 to0 f =
  cursorIndex >>= \ cur ->
  absoluteIndex (min from0 to0) >>= \ from ->
  absoluteIndex (max from0 to0) >>= \ to ->
  let contained =  safeSliceBetween from to in
  if      from <= cur && to <= cur then contained $ lift . safeSliceBetween 0 0 . f
  else if from >  cur && to >  cur then contained $ lift . safeSliceBetween 0 0 . flip f
  else do
    bef <- indexNearCursor Before
    aft <- indexNearCursor After
    safeSliceBetween from bef $ lift . safeSliceBetween aft to . f

-- | Similar to 'withRegion' but operates on the entire defined region of the buffer.
withFullSlices
  :: MonadEditVec vec m
  => (UnsafeSlice vec -> UnsafeSlice vec -> SafeEvalSlice m a)
  -> m a
withFullSlices f = evalSliceSafely $ join $ f <$> getLoSlice <*> getHiSlice

-- | Create a new vector, copy the portion of the state tracking the old vector and the cursor
-- positions onto the stack, then evaluate a continuation function of type @m@ with the new vector
-- in place. Note that the cursor is not modified when evaluation of the continuation begins, even
-- if the cursor would be out of bounds. Once the continuation function completes, restore the prior
-- state and return the new vector.
withNewVector
  :: (MonadIO m, GMVec.MVector vec elem, MonadGapBuffer (vec RealWorld elem) m)
  => VecLength -> SafeEvalSlice m () -> m (vec RealWorld elem)
withNewVector size f = do
  oldvec <- modVector id
  before <- modCount Before id
  after  <- modCount After  id
  newvec <- newVector size
  modVector (const newvec)
  () <- evalSliceSafely f
  modCount Before (const before)
  modCount After  (const after )
  modVector (const oldvec)
  return newvec

-- | Select a region of valid elements given an index and size values, taking into account the
-- position of the cursor, and copy the region into a new contiguous mutable vector that contains no
-- invalid elements. It is usually expected that the mutable vector will be frozen by the function
-- which called 'copyRegion'.
copyRegion
  :: (MonadIO m, GMVec.MVector vec elem, MonadGapBuffer (vec RealWorld elem) m)
  => VecIndex -> VecIndex -> m (vec RealWorld elem)
copyRegion from to = withRegion from to $ \ oldLo oldHi -> lift $ do
  withNewVector (sliceSize GMVec.length oldLo + sliceSize GMVec.length oldHi) $ lift $ do
    modCount Before $ const $ sliceSize GMVec.length oldLo
    modCount After  $ const $ sliceSize GMVec.length oldHi
    withFullSlices $ \ newLo newHi -> do
      mutableCopy newLo oldLo
      mutableCopy newHi oldHi

-- | Write an element to a vector index, overwriting whatever was there before. __WARNING__: there
-- is no bounds checking.
putElemIndex
  :: (MonadIO m, MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem
     , Show elem --DEBUG
     )
  => VecIndex -> elem -> m ()
putElemIndex i elem =
  ( asrtMWrite UnsafeOp "putElemIndex"
      <$> ((,) "getVector" <$> getVector)
      <*> pure (asrtShow "i" i)
      <*> pure elem
  ) >>= liftIO

-- | Read an element from a vector index. __WARNING__: there is no bounds checking.
getElemIndex
  :: (MonadIO m, MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem
     , Show elem --DEBUG
     )
  => VecIndex -> m elem
getElemIndex i =
  ( asrtMRead UnsafeOp "getElemIndex"
      <$> ((,) "getVector" <$> getVector)
      <*> pure (asrtShow "i" i)
  ) >>= liftIO

-- | Put an element at the cursor. This function evaluates (modCursorIsDefined $ const True).
putElem
  :: (MonadIO m, MonadGapBuffer (vec RealWorld elem) m, GMVec.MVector vec elem
     , Show elem --DEBUG
     )
  => elem -> m ()
putElem elem = do
  cursorIndex >>= flip putElemIndex elem
  setCursorIsDefined True

-- | Get an element from the cursor. If the element is not defined ((modCursorIsDefined id) returns
-- false), an exception is thrown.
getElem
  :: (MonadIO m, MonadGapBuffer (vec RealWorld elem) m, GMVec.MVector vec elem
     , Show elem --DEBUG
     )
  => m elem
getElem = do
  defined <- getCursorIsDefined
  if defined then cursorIndex >>= getElemIndex else throwLimitErr Before

-- | Sets the element under the cursor to an undefined value.
delElem
  :: (MonadIO m, MonadGapBuffer (vec RealWorld elem) m, GMVec.MVector vec elem
     , Show elem --DEBUG
     )
  => m ()
delElem = do
  join $ putElemIndex <$> cursorIndex <*> nullElem
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
  :: (MonadGapBuffer (vec RealWorld elem) m, GMVec.MVector vec elem)
  => VecLength -> m (Maybe (vec RealWorld elem))
prepLargerVector newsiz = do
  oldsiz <- getAllocSize
  if oldsiz >= newsiz then return Nothing else
    Just <$> newVector (minPow2ScaledSize oldsiz newsiz)

-- | If and only if the vector is full, allocate a new vector with enough space to fit the requested
-- number of elements by doubling the size of the current vector until the size is large enough,
-- then copy the elements from the current vector to the new vector, and then replace the current
-- vector with the new one.
growVector
  :: (MonadIO m, MonadGapBuffer (vec RealWorld elem) m, GMVec.MVector vec elem)
  => VecLength -> m ()
growVector increase = if increase <= 0 then return () else
  countDefined >>= \ count -> prepLargerVector (count <> increase) >>= \ case
    Nothing     -> return ()
    Just newvec -> withFullSlices $ \ oldbef oldaft -> lift $ do
      modVector $ const newvec
      withFullSlices $ \ newbef newaft ->
        mutableCopy newbef oldbef >> mutableCopy newaft oldaft

-- | Push a single element to the index 'Before' (currently on) the cursor, or the index 'After' the
-- cursor, and then shift the cursor to point to the pushed element.
pushElem
  :: (MonadIO m, MonadGapBuffer (vec RealWorld elem) m, GMVec.MVector vec elem
     , Show elem --DEBUG
     )
  => RelativeToCursor -> elem -> m ()
pushElem rel elem = growVector 1 >> modCount rel (+ 1) >>= \ i -> case rel of
  Before -> putElem elem
  After  -> putElemIndex (indexAfterRange 0 i) elem

-- | Push a vector of elements starting at the index 'Before' (currently on) the cursor, or the
-- index 'After' the cursor.
pushElemVec
  :: (MonadIO m, GVec.Vector vec elem, MonadGapBuffer (GVec.Mutable vec RealWorld elem) m
     , Show elem --DEBUG
     )
  => RelativeToCursor -> vec elem -> m ()
pushElemVec rel src = do
  let len = VecLength $ GVec.length src
  growVector len
  ok <- withVoidSlice (case rel of{ Before -> negate len; After -> len; }) $ \ targ ->
    lift (modCount rel (+ len)) >> copy targ (UnsafeSlice src)
  unless ok $ error $ "pushElemVec: internal error, \"growVector\" failed to create space"

-- | Pop a single element from the index 'Before' (currently on) the cursor, or from the index 'After'
-- the cursor, and then shift the cursor to point to the pushed element.
popElem
  :: (MonadIO m, MonadGapBuffer (vec RealWorld elem) m, GMVec.MVector vec elem
     , Show elem --DEBUG
     )
  => RelativeToCursor -> m elem
popElem = \ case
  Before -> getElem <* delElem
  After  -> do
    count <- modCount After id
    if count <= 0 then throwLimitErr After else do
      siz <- getAllocSize
      getElemIndex (indexAtRangeEnd (indexAfterRange 0 siz) (negate count))
        <* modCount After (subtract 1)

-- | Move the cursor, which will shift elements around the vector, using a algorithm of O(n) steps,
-- where @n@ is the 'Relative' shift value. Pass a boolean value indicating whether or not you would
-- like to perform bounds checking, if so an exception will be raised if the line index goes out of
-- bounds.
shiftCursor
  :: (MonadIO m, MonadGapBuffer (GVec.Mutable vec RealWorld elem) m,
      GMVec.MVector (GVec.Mutable vec) elem,
      GVec.Vector vec elem
     , Show elem --DEBUG
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
      else evalSliceSafely $ do
        from <- sliceFromCursor count
        to   <-
          if      count >  1 then sliceVectorSafe lo count
          else if count < -1 then sliceVectorSafe (indexAfterRange (hi + 1) count) (negate count)
          else error "shiftCursor: internal error, this should never happen"
        mutableMove to from
        lift done
    where
      count  = unwrapRelative count0
      done   = do
        before <- modCount Before (+ count)
        modCount After (subtract count)
        when (before == 0) $ setCursorIsDefined False

-- | Copy the current mutable buffer vector to an immutable vector.
freezeVector
  :: (MonadIO m, GVec.Vector vec elem, MonadGapBuffer (GVec.Mutable vec RealWorld elem) m)
  => m (vec elem)
freezeVector = do
  oldvec <- getVector
  newvec <- countDefined >>= newVector
  withFullSlices $ \ oldLo oldHi -> do
    lift $ do
      modVector (const newvec)
      withFullSlices $ \ newLo newHi -> mutableCopy newLo oldLo >> mutableCopy newHi oldHi
      modVector (const oldvec)
    safeFreeze (UnsafeSlice newvec)

-- | Creates a deep-copy of a vector with a cursor, in which elements before the
-- cursor are aligned at the start of the vector, and elements after the cursor are aligned at the
-- end of the vector.
copyVec
  :: (GMVec.MVector vector a, PrimMonad m)
  => vector (PrimState m) a
  -> VecLength -> VecLength -> m (vector (PrimState m) a)
copyVec oldVec (VecLength before) (VecLength after) = do
  let len = VecLength $ GMVec.length oldVec
  let (VecIndex upper) = indexAfterRange 0 $ len - VecLength after
  newVec <- GMVec.new (fromLength len)
  when (before > 0) $ GMVec.copy
    (GMVec.unsafeSlice 0 before newVec)
    (GMVec.unsafeSlice 0 before oldVec)
  when (after  > 0) $ GMVec.copy
    (GMVec.unsafeSlice upper after newVec)
    (GMVec.unsafeSlice upper after oldVec)
  return newVec
