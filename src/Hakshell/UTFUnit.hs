-- | An unsigned 11-bit word, allowing integer values between 0 and @2^11-1 == 2048@. Since UTF
-- characters require 2^21 bits to store, a 'Data.Word.Word32' value can store one full UTF
-- character, and one 11 bit word. In a text editing buffer, each character can be associated with a
-- 'Word11' value, allowing you to apply tags directly in-line with the text in memory.
module Hakshell.UTFUnit
  ( UTFUnit, UTFUnitTag, LineBreakTag,
    Word11, word11to16, packUTFUnit, unpackUTFUnit, unpackUTFUnitOrd,
    bitSizeWord11, mask16Word11, mask32Word11, mask32Char,
    -- * Bitwise Operators and Printers
    -- $Bitwise
    BitPartition, BitRotation,
    rotateLowBits, showBin, fillBits,
  ) where

import           Control.Arrow
import           Control.Monad

import           Data.Bits
import           Data.Char
import           Data.Data
import           Data.Function
import           Data.Ix
import           Data.Word

import           Foreign.Ptr
import           Foreign.Storable

----------------------------------------------------------------------------------------------------

-- | A 'UTFUnit' is a 32-bit unsigned word which encodes two serialized values paired together. The
-- first value is an ordary 'Prelude.Char' value, which according to the Haskell report, is a UTF
-- character.
--
-- A UTF character, at the time of this writing, only requires 21 bits of information, so the lower
-- (as in least-significant) bits of the encoded 'Data.Word.Word32' value encode the UTF character.
--
-- The remaining 11 bits of the encoded 'Data.Word.Word32' value contains an 'Word11' value (an
-- 11-bit word) which can be used to store an arbitrary tag value along with the UTF character. This
-- allows for syntax coloring.
type UTFUnit = Word32

-- | It is possible to associate a tag with a 'Text.TokenParser.LineBreak.LineBreakSym' on each line
-- in a 'Text.Editor.IOBuffer.IOBuffer'.
type LineBreakTag = Word11

-- | Unit character tag: when constructing a new 'Text.Editor.IOBuffer.IOBuffer', this value
-- defines the tag associated with all incomming characters.
type UTFUnitTag = Word11

----------------------------------------------------------------------------------------------------

-- | An 11-bit word, which is stored in the upper 11 bits of information of a 'Data.Word.Word32'
-- data value, since these 11 bits are not used when storing a 21-bit Unicode character. This allows
-- a text buffer to use a uniform 32-bits for every character which guarantees O(1) indexing and
-- updates, while also providing 11 bits of space for tagging each character in the buffer.
--
-- The tagging information attached to each character is defined by you, the programmer. 11 bits
-- provides @2^11 == 2048@ unique 'Prelude.Enum'erable values. Any arbitrary information may be
-- encoded as this tagging information can be serialized to a 'Word11' value. This may be useful for
-- features like syntax coloring or tab completion. These values are also only stored in memory
-- unless explicitly extracted. For any ordinary text maniupulation operation, once characters are
-- moved out of a 'Text.Editor.IOBuffer.IOBuffer', the 'Word11' tagging is stripped away, and can
-- only be added back when re-inserting characters.
--
-- Internally this data type is an ordinary 'Data.Word.Word16' and instantiates almost all of the
-- same type classes as 'Data.Word.Word16', so values can be manipulated in exactly the same way you
-- would manipulate most other 'Prelude.Bounded', 'Prelude.Integral', 'Prelude.Real'-valued,
-- 'Prelude.Enum'erable, 'Prelude.Num'eric series of 'Data.Bits.Bits'.
newtype Word11 = Word11 { word11to16 :: Word16 } deriving (Eq, Ord, Data)

instance Bounded Word11 where { minBound = Word11 0; maxBound = Word11 mask16Word11; }

instance Show Word11 where { show (Word11 a) = show a; }

instance Read Word11 where { readsPrec p = liftM (first toEnum) . readsPrec p; }

instance Real Word11 where { toRational = toRational . word11to16; }

instance Enum Word11 where
  succ w11@(Word11 i) =
    if w11==maxBound
     then error "Enum.succ{Word11}: tried to take `succ' of maxBound"
     else Word11 (i+1)
  pred w11@(Word11 i) =
    if w11==minBound
     then error "Enum.succ{Word11}: tried to take `pred' of minBound"
     else Word11 (i-1)
  toEnum  = Word11 . flip mod mask16Word11 . toEnum
  fromEnum (Word11 i) = fromEnum i
  enumFrom       i                  = enumFromThen i (Word11 1)
  enumFromThen   i (Word11 step)    = takeWhile (<= maxBound) $ iterate (Word11 . (+ step) . word11to16) i
  enumFromTo     i              top = takeWhile (<= top) $ enumFrom i
  enumFromThenTo i         step top = takeWhile (<= top) $ enumFromThen i step

instance Bits Word11 where
  (Word11 a) .&. (Word11 b) = Word11 $ a .&. b
  (Word11 a) .|. (Word11 b) = Word11 $ a .|. b
  xor (Word11 a) (Word11 b) = Word11 $ xor a b
  complement (Word11 a) = Word11 $ complement a .&. mask16Word11
  zeroBits = Word11 zeroBits
  shift (Word11 a) i = Word11 $ shift a i
  rotate (Word11 a) = Word11 . rotateLowBits bitSizeWord11 a
  --rotate (Word11 a) i =
  --  let n = mod i bitSizeWord11
  --      b = shift (fromIntegral a :: Word32) n
  --      mask = (.&.) (complement $ shift 1 n - 1)
  --  in Word11 $ if n==0 then a else mask16Word11 .&. fromIntegral (mask b .|. shift b (negate bitSizeWord11))
  bit i = if i>=bitSizeWord11 then zeroBits else Word11 $ bit i
  testBit (Word11 a) i = testBit a i
  bitSize _ = bitSizeWord11
  bitSizeMaybe _ = Just bitSizeWord11
  isSigned _ = False
  popCount (Word11 a) = popCount a

instance Num Word11 where
  (Word11 a) + (Word11 b) = toEnum (fromEnum a + fromEnum b)
  (Word11 a) - (Word11 b) = toEnum (fromEnum a - fromEnum b)
  (Word11 a) * (Word11 b) = toEnum (fromEnum a * fromEnum b)
  negate = complement
  abs = id
  signum (Word11 i) = Word11 $ if i==0 then 0 else 1
  fromInteger = Word11 . (.&. mask16Word11) . fromInteger

instance Integral Word11 where
  quotRem (Word11 a) (Word11 b) = Word11 *** Word11 $ quotRem a b
  toInteger (Word11 a) = toInteger a

instance FiniteBits Word11 where
  finiteBitSize _ = bitSizeWord11
  countLeadingZeros (Word11 a) = countLeadingZeros a - 5
  countTrailingZeros (Word11 a) = countTrailingZeros a

instance Ix Word11 where
  range ((Word11 a), (Word11 b)) = fmap Word11 $ range (a, b)
  index = ((.) fromEnum) . subtract . uncurry min
  inRange (a, b) x = a<=x && x<=b

instance Storable Word11 where
  sizeOf  _ = sizeOf (0::Word16)
  alignment = sizeOf
  peek = liftM (Word11 . (.&. mask16Word11)) . peek . castPtr
  poke ptr (Word11 a) = poke (castPtr ptr) a

----------------------------------------------------------------------------------------------------

mask16Word11 :: Word16
mask16Word11 = shift 1 11 - 1

mask32Word11 :: Word32
mask32Word11 = complement mask32Char

mask32Char :: Word32
mask32Char = shift 1 21 - 1

pack32CharWord11Shift :: Int
pack32CharWord11Shift = finiteBitSize (minBound::Word32) - finiteBitSize (minBound::Word11)

bitSizeWord11 :: Int
bitSizeWord11 = 11

-- | Pack a 'Word11' and 'Prelude.Char' value together into a single 'Data.Word.Word32' value.
packUTFUnit :: Word11 -> Char -> UTFUnit
packUTFUnit (Word11 w) c = fromIntegral (ord c) .|.
  shift (fromIntegral w) pack32CharWord11Shift

-- | Unpack a 'Word11' and a 'Prelude.Char' which were packed together using 'packUTFUnit'.
unpackUTFUnit :: UTFUnit -> (Word11, Char)
unpackUTFUnit = fmap chr . unpackUTFUnitOrd

-- | Unpack a 'Word11' and a 'Prelude.Char' which were packed together using 'packUTFUnit', however
-- the 'Prelude.Char' is returned as an 'Prelude.Int'. This is useful if you suspect your 'UTFUnit'
-- contains an invalid character, and you want to check it before evaluating it with the
-- 'Data.Char.chr' function in the "Data.Char" module.
unpackUTFUnitOrd :: UTFUnit -> (Word11, Int)
unpackUTFUnitOrd w =
  ( Word11 $ fromIntegral $ shift w $ negate pack32CharWord11Shift
  , fromIntegral $ mask32Char .&. w
  )

----------------------------------------------------------------------------------------------------

-- $Bitwise
-- These are functions that do not belong in this module, or in any module anywhere in this entier
-- package. But they are useful, so until we can find a home for them, they stay here.

type BitPartition = Int
type BitRotation  = Int

-- | Convert the bottom-most bit to a character. Pass a 'Prelude.Char'aracter to be used to
-- represent a zero, and a 'Prelude.Char'acter to be used to represent a one.
showBit0 :: (Eq a, Bits a) => Char -> Char -> a -> Char
showBit0 o i a = if a .&. bit 0 /= zeroBits then i else o

-- | Shows a value of type that is an instance of 'Data.Bits.Bits' as a sequence of binary digits
-- with a partition, as printed by the 'showBit0' function. The first 'Prelude.Char'aracter is a
-- partition character which you must define in order to show the 'BitPartition'.
--
-- The 'BitPartition' value is usually set to the midpoint of the word, so for example, when
-- calling this function on a value of type 'Data.Word.Word32', the 'BitPartition' should be 16,
-- calling this function on a value of type 'Data.Word.Word8', the 'BitPartition' should be 4. The
-- only exception is when showing the result of 'rotateLowBits', in which case you should pass the
-- 'BitPartition' value that was passed to 'rotateLowBits'.
showBinWith :: (Eq a, Bits a) => Char -> Char -> Char -> BitPartition -> a -> [Char]
showBinWith s o i part x = reverse $ (\ (a,b) -> a++s:b) $ splitAt part $ case bitSizeMaybe x of
  Just siz -> take siz $ flip fix x $ \ loop x -> showBit0 o i x : loop (x `shiftR` 1)
  Nothing  -> flip fix x $ \ loop x ->
    showBit0 o i x : if zeroBits == x then "" else loop (x `shiftR` 1)

-- | A simplified version of 'showBinWith' with character values defaulting to 
showBin :: (Eq a, Bits a) => BitPartition -> a -> [Char]
showBin = showBinWith '_' '0' '1'

-- | Creates a bit mask where all bit positions between the two given integers are set using 'Data.Bits.setBit'.
fillBits :: Bits a => Int -> a
fillBits a = if a == 0 then zeroBits else foldl setBit zeroBits [0 .. a-1]

-- | Perform a bit 'Data.Bits.rotate' operation __only__ on the lowest @n::'Prelude.Int'@ bits.
rotateLowBits :: Bits a => BitPartition -> a -> BitRotation -> a
rotateLowBits part x rot =
  if part <= 0
   then error $ "rotateLowBits invalid parameter: BitPartiton = "++show part
   else let posRot = mod rot part in if part < 2 || posRot == 0 then x else
    let negRot = posRot - part
        mask   = fillBits part
        loMask = fillBits (abs negRot)
        result = shift (x .&. loMask) posRot
             .|. shift (x .&. mask) negRot
             .|.  xor (x .|. mask) mask
        --using ('xor', (.|.)) because we can't use ('complement', (.&.)) when (a::Integer)
    in  case bitSizeMaybe x of
          Just siz -> case compare part siz of
            LT -> result
            EQ -> rotate x posRot
            GT -> error $
              "bit arithmetic error: bit partition "++show part++
              " greater than word size "++show siz
          Nothing  -> result
    --
    -- ### An illustration of this algorithm ###
    --
    -- Lets evaluate (rotateLowBits 13 x 10) where 'x' is an arbitrary 16-bit word (using C-language
    -- bitwise operators: and=(&), or=(|), complement=(~), shiftL=(<<), shiftR=(>>))...
    -- 
    -- FEDCBA9876543210
    --    <BA987654321>  partition P = sizeof(0x0,0xC) = 13 (delimited by angle brackets)
    --    21><BA9876543  (desired result) rotated R=10 within the partition P
    --    21>            let R' = R-P = -3; loMask = fill (abs R'); in x&loMask << R
    --       <BA9876543  mask&x << R == mask&x >> abs R'
    --    21><BA9876543  desired result == mask&x << R' | x&loMask << R | ~mask&x
    -- 
    -- Above, R is posRot, R' is negRot, fill is fillBits

