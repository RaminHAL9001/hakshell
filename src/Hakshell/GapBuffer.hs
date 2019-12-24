-- | This module implements a generic, mutable vector with a cursor. Moving the cursor shifts
-- elements after the cursor toward the end of the vector, ensuring pushing elements is always an
-- O(1) operation as long as there is space available. Moving the cursor is an O(n) operation, where
-- /n/ is the distance of the cursor movement.
module Hakshell.GapBuffer
  ( IsIndex(..), IsLength(..), IndexedState, HasOpposite(..),
    VecIndex(..), VecLength(..), indexAfterRange, indexAtRangeEnd, iter2way,
    Relative(..), Absolute(..), RelativeToCursor(..), relative, absolute,
    MonadEditVec(..), getVector,
    indexNearCursor, cursorIndex, cursorAtEnd, absoluteIndex, relativeIndex, getVoid,
    getAllocSize, countOnCursor, countDefined, countUndefined,
    sliceFromCursor, sliceBetween, getVoidSlice, getLoSlice, getHiSlice, withRegion, copyRegion,
    putElemIndex, getElemIndex, putElem, getElem, delElem,
    pushElem, pushElemVec, popElem, shiftCursor,
    minPow2ScaledSize, prepLargerVector, growVector, freezeVector, copyVec,
  ) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Primitive

import qualified Data.Vector.Generic         as GVec
import qualified Data.Vector.Generic.Mutable as GMVec

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

asrtGSlice
  :: (vec -> Int)
  -> (Int -> Int -> vec -> vec)
  -> (Int -> Int -> vec -> vec)
  -> UseSafeOp -> FunctionName
  -> LabeledValue VecIndex -> LabeledValue VecLength -> LabeledValue vec -> vec
asrtGSlice length safe unsafe safety funcName (ilbl, VecIndex i) (sizlbl, VecLength siz) (veclbl, vec) =
  let len = ("(length "++veclbl++")", length vec) in
  let top = ( "(indexAfterRange "++ ilbl ++ ' ' : sizlbl ++ ")", i + siz) in
  assert funcName
    [asrtZero `le` (ilbl, i), (ilbl, i) `le` len, asrtZero `le` top, top `le` len]
    ((if not unsafeMode || safety == SafeOp then safe else unsafe) i siz vec)
{-# INLINE asrtGSlice #-}

asrtSlice
  :: GVec.Vector vec elem
  => UseSafeOp -> FunctionName
  -> LabeledValue VecIndex -> LabeledValue VecLength -> LabeledValue (vec elem) -> vec elem
asrtSlice = asrtGSlice GVec.length GVec.slice GVec.unsafeSlice
{-# INLINE asrtSlice #-}

asrtMSlice
  :: (GMVec.MVector vec elem)
  => UseSafeOp -> FunctionName
  -> LabeledValue VecIndex -> LabeledValue VecLength -> LabeledValue (vec st elem) -> vec st elem
asrtMSlice = asrtGSlice GMVec.length GMVec.slice GMVec.unsafeSlice
{-# INLINE asrtMSlice #-}

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

asrtMRead
  :: (PrimMonad m, GMVec.MVector vec elem)
  => UseSafeOp -> FunctionName
  -> LabeledValue (vec (PrimState m) elem) -> LabeledValue VecIndex -> m elem
asrtMRead = asrtGReadWrite GMVec.length GMVec.read GMVec.unsafeRead
{-# INLINE asrtMRead #-}

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

asrtOne :: Num n => LabeledValue n
asrtOne = ("1", 1)

----------------------------------------------------------------------------------------------------

class IsIndex i where
  fromIndex :: i -> Int
  toIndex :: Int -> i

class IsLength i where
  fromLength :: i -> Int
  toLength :: Int -> i

class Monad m => IndexedState idx m | m -> idx where {}

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

unwrapAbsolute :: IsIndex i => Absolute i -> VecIndex
unwrapAbsolute (Absolute i) = toIndex $ fromIndex i

absolute :: IsIndex i => VecIndex -> Absolute i
absolute = Absolute . toIndex . fromIndex

unwrapRelative :: IsLength i => Relative i -> VecLength
unwrapRelative (Relative i) = toLength $ fromLength i

relative :: IsLength i => VecLength -> Relative i
relative = Relative . toLength . fromLength

----------------------------------------------------------------------------------------------------

-- | Used mostly internally to indicate an index that can be used to access an element in the vector
-- of a gap buffer, as opposed to an index relative to the cursor, or relative to the gap.
newtype VecIndex = VecIndex Int
  deriving (Eq, Ord, Show, Read, Enum, Num, Bounded)

instance IsIndex VecIndex where
  fromIndex (VecIndex i) = i
  toIndex = VecIndex

-- | Used mostly internally to indicate the size of a vector of a gap buffer, or the size of a slice
-- of a the vector of a gap buffer, as opposed to an index relative to the cursor, or relative to
-- the gap.
newtype VecLength = VecLength Int
  deriving (Eq, Ord, Show, Read, Enum, Num, Bounded)

instance Semigroup VecLength where { (<>) = (+); }
instance Monoid    VecLength where { mempty = 0; mappend = (<>); }
instance IsLength  VecLength where
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

-- | Like the @[a .. b]@ notation when @(a <= b)@, but iterates in decreacing order when @(a > b)@.
iter2way :: (Ord n, Num n) => n -> n -> [n]
iter2way from to =
  takeWhile (flip (if from <= to then (<=) else (>=)) to) $
  iterate ((if from <= to then (+) else subtract) 1) from

----------------------------------------------------------------------------------------------------

-- Line indexing arithmetic
--
-- I think of these functions as being similar to named environment variables in that you can use
-- these names to have meaningful symbols for certain vector indicies. This makes code involving
-- ranges of lines, and code involving translating user-facing @('Absolute' 'LineIndex')@ values to
-- simplliied arithmetic expressions that I consuder to be more human readable.

class MonadIO m => MonadEditVec vec m | m -> vec where
  nullElem  :: vec ~ v elem => m elem
  newVector :: VecLength -> m vec
  modVector :: (vec -> vec) -> m vec
  -- | This is a value relative to the cursor, the integer value returned does not include the value
  -- under the cursor itself, as the value under the cursor may or may not exist.
  modCount  :: RelativeToCursor -> (VecLength -> VecLength) -> m VecLength
  -- | Returns a boolean indicating whether the element under the cursor is valid.
  modCursorIsDefined :: (Bool -> Bool) -> m Bool
  throwLimitErr :: RelativeToCursor -> m void
  throwIndexErr :: VecIndex -> m void
  throwCountErr :: VecLength -> m void

-- | The vector of the text buffer.
getVector :: MonadEditVec vec m => m vec
getVector = modVector id

-- | When placing an element into the buffer, this function can returns the index in the array
-- 'Before' (or "under") the cursor, or the index 'After' the cursor, which are on either end of the
-- gap in the gap buffer.
indexNearCursor
  :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem)
  => RelativeToCursor -> m VecIndex
indexNearCursor = \ case
  Before -> indexAfterRange 0 <$> modCount Before id
  After  -> indexAtRangeEnd 0 <$> (subtract <$> modCount After id <*> getAllocSize)

-- | The size of the buffer allocation
getAllocSize :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem) => m VecLength
getAllocSize = VecLength . GMVec.length <$> getVector

-- | The number of valid elements under the cursor, this number is either 0 or 1.
countOnCursor :: MonadEditVec vec m => m VecLength
countOnCursor = (\ a -> VecLength $ if a then 0 else 1) <$> modCursorIsDefined id

-- | Calls 'modCount' but adds 'countOnCursor' if you are counting elements 'Before' the cursor.
countCursor :: MonadEditVec vec m => RelativeToCursor -> m VecLength
countCursor = \ case
  Before -> (+) <$> countOnCursor <*> modCount Before id
  After  -> modCount After id

-- | Returns the total number of defined elements.
countDefined :: MonadEditVec vec m => m VecLength
countDefined = (<>) <$> countCursor Before <*> countOnCursor

-- | The number of elements in the buffer that are not valid, @(bufAllocSize - bufLineCount)@
countUndefined
  :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem)
  => m VecLength
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
absoluteIndex
  :: (IsIndex i, IndexedState i m, MonadEditVec (vec st elem) m, GMVec.MVector vec elem)
  => Absolute i -> m VecIndex
absoluteIndex = unwrapAbsolute >>> \ i -> indexNearCursor Before >>= \ cur ->
  if i <= cur then pure i else indexAfterRange i <$> countUndefined

-- | Get the index within the vector that is associated with the given index value 'Relative' to the
-- cursor.
relativeIndex
  :: (IsLength i, IndexedState i m, MonadEditVec (vec st elem) m, GMVec.MVector vec elem)
  => Relative i -> m VecIndex
relativeIndex = (. unwrapRelative) $ \ i ->
  flip indexAfterRange i <$> indexNearCursor (if i <= 0 then Before else After)

-- | Return the starting and ending index of the void region of the buffer, which is the contiguous
-- region of undefined elements within the buffer.
getVoid
  :: (MonadEditVec (mvec st elem) m, GMVec.MVector mvec elem)
  => m (Maybe (VecIndex, VecIndex))
getVoid = do
  rgn <- (,) <$> ((+ 1) <$> indexNearCursor Before) <*> (subtract 1 <$> indexNearCursor After)
  return $ guard (uncurry (<=) rgn) >> Just rgn

-- | Make a slice of elements relative to the current cursor. A negative argument will take elements
-- before and up-to the cursor, a positive argument will take that many elements starting from
-- 'getLineAfterCur'. Pass a boolean value indicating whether or not you would like to perform
-- bounds checking, if so an exception will be raised if the line index goes out of bounds.
sliceFromCursor
  :: (MonadEditVec (mvec st elem) m, GMVec.MVector mvec elem)
  => VecLength -> m (mvec st elem)
sliceFromCursor count = getVector >>= \ vec ->
  if count < 0 then indexNearCursor Before >>= \ i ->
    return $ asrtMSlice SafeOp "sliceFromCursor"
      (asrtShow "indexAfterRange i count" $ indexAfterRange i count)
      (asrtShow "abs count" $ abs count)
      ("vec", vec)
  else if count > 0 then indexNearCursor After >>= \ i ->
    return $ asrtMSlice SafeOp "sliceFromCursor"
      (asrtShow "i" i)
      (asrtShow "count" count)
      ("vec", vec)
  else return $ GMVec.slice 0 0 vec
{-# INLINE sliceFromCursor #-}

sliceBetween
  :: (MonadEditVec (mvec st elem) m, GMVec.MVector mvec elem)
  => VecIndex -> VecIndex -> m (mvec st elem)
sliceBetween from to = getVector <&> \ vec ->
  asrtMSlice SafeOp "sliceBetween"
    (asrtShow "from" from)
    (asrtShow "indexDistance from to" $ indexDistance from to)
    ("vec", vec)

-- | Make a slice of the void region of the buffer (the contiguous region of invalid elements after
-- the cursor). This can be used as the target of a vector copy. If the buffer is full, 'Nothing' is
-- returned.
getVoidSlice
  :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem)
  => VecLength -> m (Maybe (vec st elem))
getVoidSlice count = do --TODO: double-check this function
  vec <- (,) "getVector" <$> getVector
  vsp <- getVoid
  return $ vsp <&> \ (lo, hi) ->
   if count < 0
   then asrtMSlice SafeOp "getVoidSlice"
          (asrtShow "indexAfterRange hi count" $ indexAfterRange hi count)
          (asrtShow "abs count" $ abs count)
          vec
   else if count > 0
   then asrtMSlice SafeOp "getVoidSlice"
          (asrtShow "lo" lo)
          (asrtShow "count" count)
          vec
   else asrtMSlice SafeOp "getVoidSlice" asrtZero asrtZero vec

-- | Obtain a slice (using 'sliceFromCursor') for the portion of the vector containing elements
-- before or on the current cursor.
getLoSlice :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem) => m (vec st elem)
getLoSlice = countCursor Before >>= sliceFromCursor . negate

-- | Obtain a slice (using 'sliceFromCursorM') for the portion of the vector containing elements
-- after the current cursor.
getHiSlice
  :: (MonadEditVec (vec st elem) m, GMVec.MVector vec elem)
  => m (vec st elem)
getHiSlice = countCursor After >>= sliceFromCursor

-- | Given a region, slice the vector to the region and evaluate one of two given continuations with
-- the slice. If the region spans the gap in the buffer, the first continuation is called. If the
-- region is fully contained to within either sub region on either side of the gap, then the second
-- continuation function is called.
withRegion
  :: (IsIndex i, IndexedState i m, GMVec.MVector vec elem, MonadEditVec (vec st elem) m)
  => (vec st elem -> m a)
  -> (vec st elem -> vec st elem -> m a)
  -> Absolute i -> Absolute i
  -> m a
withRegion contained straddle from0 to0 = do
  vec  <- getVector
  cur  <- cursorIndex
  from <- absoluteIndex from0
  to   <- absoluteIndex to0
  if from <= cur && to <= cur
   then contained $ asrtMSlice SafeOp "withRegion/below"
          (asrtShow "from" from)
          (asrtShow "indexDistance from to" $ indexDistance from to)
          ("vec", vec)
   else if from > cur && to > cur
   then contained $ asrtMSlice SafeOp "withRegion/above"
          (asrtShow "from" from)
          (asrtShow "indexDistance from to" $ indexDistance from to)
          ("vec", vec)
   else do
    bef <- indexNearCursor Before
    aft <- indexNearCursor After
    join $ straddle <$> sliceBetween from bef <*> sliceBetween aft  to

-- | Select a region of valid elements given an index and size values, taking into account the
-- position of the cursor, and copy the region into a new contiguous mutable vector that contains no
-- invalid elements. It is usually expected that the mutable vector will be frozen by the function
-- which called 'copyRegion'.
copyRegion
  :: (IsIndex i, IndexedState i m, GMVec.MVector vec elem, MonadEditVec (vec RealWorld elem) m)
  => Absolute i -> Absolute i -> m (vec RealWorld elem)
copyRegion = withRegion return $ \ lovec hivec -> do
  let myname = "copyRegion"
  let losiz = VecLength $ GMVec.length lovec
  let hisiz = VecLength $ GMVec.length hivec
  newvec <- newVector $ losiz + hisiz
  let copy  = (.) liftIO . if unsafeMode then GMVec.unsafeCopy else GMVec.copy
  flip copy lovec $ asrtMSlice SafeOp myname
    asrtZero (asrtShow "losiz" losiz) ("newvec", newvec)
  flip copy hivec $ asrtMSlice SafeOp myname
    (asrtShow "indexAfterRange 0 losiz" $ indexAfterRange 0 losiz)
    (asrtShow "hisiz" hisiz)
    ("newvec", newvec)
  return newvec

-- | Write an element to a vector index, overwriting whatever was there before. __WARNING__: there
-- is no bounds checking.
putElemIndex
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem
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
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem
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
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem
     , Show elem --DEBUG
     )
  => elem -> m ()
putElem elem = do
  cursorIndex >>= flip putElemIndex elem
  void $ modCursorIsDefined $ const True

-- | Get an element from the cursor. If the element is not defined ((modCursorIsDefined id) returns
-- false), an exception is thrown.
getElem
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem
     , Show elem --DEBUG
     )
  => m elem
getElem = do
  defined <- modCursorIsDefined id
  if defined then cursorIndex >>= getElemIndex else throwLimitErr Before

-- | Sets the element under the cursor to an undefined value.
delElem
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem
     , Show elem --DEBUG
     )
  => m ()
delElem = do
  join $ putElemIndex <$> cursorIndex <*> nullElem
  void $ modCursorIsDefined $ const False

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
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem)
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
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem)
  => VecLength -> m ()
growVector increase = if increase <= 0 then return () else
  countDefined >>= \ count -> prepLargerVector (count <> increase) >>= \ case
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

-- | Push a single element to the index 'Before' (currently on) the cursor, or the index 'After' the
-- cursor, and then shift the cursor to point to the pushed element.
pushElem
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem
     , Show elem --DEBUG
     )
  => RelativeToCursor -> elem -> m ()
pushElem rel elem = growVector 1 >> modCount rel (+ 1) >>= \ i -> case rel of
  Before -> putElem elem
  After  -> putElemIndex (indexAfterRange 0 i) elem

-- | Push a vector of elements starting at the index 'Before' (currently on) the cursor, or the
-- index 'After' the cursor.
pushElemVec
  :: (GVec.Vector vec elem, MonadEditVec (GVec.Mutable vec RealWorld elem) m
     , Show elem --DEBUG
     )
  => RelativeToCursor -> vec elem -> m ()
pushElemVec rel src = do
  let len = VecLength $ GVec.length src
  let copy = if unsafeMode then GVec.unsafeCopy else GVec.copy
  growVector len
  targ <- getVoidSlice (case rel of { Before -> negate len; After -> len }) >>= \ case
    Nothing   -> error $ "pushElemVec: internal error, \"growVector\" failed to create space"
    Just targ -> return targ
  liftIO $ copy targ src
  void $ modCount rel (+ len)

-- | Pop a single element from the index 'Before' (currently on) the cursor, or from the index 'After'
-- the cursor, and then shift the cursor to point to the pushed element.
popElem
  :: (MonadEditVec (vec RealWorld elem) m, GMVec.MVector vec elem
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
  :: (IsLength i, IndexedState i m, GMVec.MVector vec elem, MonadEditVec (vec RealWorld elem) m
     , Show elem --DEBUG
     )
  => Relative i -> m ()
shiftCursor count0 =
  if count == 0 then return () else getVoid >>= \ case
    Nothing -> done
    Just ~(lo, hi) ->
      if      count ==  1 then popElem After  >>= pushElem Before
      else if count == -1 then popElem Before >>= pushElem After
      else do
        vec  <- getVector
        from <- sliceFromCursor count
        let to = vec &
              if      count > 1 then asrtMSlice UnsafeOp myname
                (asrtShow "lo" lo)
                (asrtShow "count" count) . (,) "getVector"
              else if count < -1 then asrtMSlice UnsafeOp myname
                (asrtShow "indexAfterRange (hi+1) count" $ indexAfterRange (hi + 1) count)
                (asrtShow "negate count" $ negate count) . (,) "getVector"
              else error "shiftCursor: internal error, this should never happen"
        liftIO $ GMVec.move to from
        done
    where
      count  = unwrapRelative count0
      myname = "shiftCursor"
      done   = do
        before <- modCount Before (+ count)
        modCount After (subtract count)
        when (before == 0) $ void $ modCursorIsDefined $ const False

-- | Copy the current mutable buffer vector to an immutable vector.
freezeVector
  :: forall vec elem m
  . (GVec.Vector vec elem, MonadEditVec (GVec.Mutable vec RealWorld elem) m
    , Show (vec elem) --DEBUG
    )
  => m (vec elem)
freezeVector = do
  newvec <- countDefined >>= newVector
  bef <- getLoSlice
  aft <- getHiSlice
  liftIO $ do
    flip GMVec.copy bef $ asrtMSlice UnsafeOp "freezeVector"
      asrtZero
      (asrtShow "length bef" $ VecLength $ GMVec.length bef)
      ("newvec", newvec)
    flip GMVec.copy aft $ asrtMSlice UnsafeOp "freezeVector"
      (asrtShow "length bef" $ indexAfterRange 0 $ VecLength $ GMVec.length bef)
      (asrtShow "length aft" $ VecLength $ GMVec.length aft)
      ("newvec", newvec)
    GVec.unsafeFreeze newvec

-- | Creates a deep-copy of a vector with a cursor, in which elements before the
-- cursor are aligned at the start of the vector, and elements after the cursor are aligned at the
-- end of the vector.
copyVec
  :: (IsIndex i, IndexedState i m, GMVec.MVector vector a, PrimMonad m)
  => vector (PrimState m) a
  -> VecLength -> VecLength -> m (vector (PrimState m) a)
copyVec oldVec0 before0 after0 = do
  let myname = "copyVec"
  let oldVec = ("oldVec", oldVec0)
  let len    = VecLength $ GMVec.length $ snd oldVec
  let upper  = asrtShow "indexAfterRange 0 (len - after)" $
                 indexAfterRange 0 $ len - after0
  let before = asrtShow "before" before0
  let after  = asrtShow "after"  after0
  newVec <- (,) "newVec" <$> GMVec.new (fromLength len)
  when (snd before > 0) $ GMVec.copy
    (asrtMSlice UnsafeOp myname asrtZero before newVec)
    (asrtMSlice UnsafeOp myname asrtZero before oldVec)
  when (snd after  > 0) $ GMVec.copy
    (asrtMSlice UnsafeOp myname upper after newVec)
    (asrtMSlice UnsafeOp myname upper after oldVec)
  return $ snd newVec
