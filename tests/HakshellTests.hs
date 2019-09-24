module Main where

import           Hakshell.String
import           Hakshell.TextEditor

import           Data.IORef
import           Data.List (tails, (!!))

import           Control.Monad.Except

import           Test.Hspec

----------------------------------------------------------------------------------------------------

main :: IO ()
main = hspec $ describe "hakshell" $ do
  lineEditorPreTests
  textEditorPreTests
  lineEditorTests
  textViewTests
  cursorMotionTests
  textDeletionTests

----------------------------------------------------------------------------------------------------

type Tags = ()

mkLoc :: Int -> Int -> TextLocation
mkLoc a b = TextLocation
  { theLocationLineIndex = Absolute $ LineIndex a
  , theLocationCharIndex = Absolute $ CharIndex b
  }

showLoc :: TextLocation -> String
showLoc
  (TextLocation
   {theLocationLineIndex=Absolute(LineIndex line)
   ,theLocationCharIndex=Absolute (CharIndex char)
   }) = '(' : show line ++ ':' : show char ++ ")"

testTextEditor :: (String -> IO a) -> TextBuffer tags -> EditText tags IO a -> IO a
testTextEditor onErr buf f = runEditTextIO f buf >>= \ case
  Left err -> onErr $ show err
  Right a  -> return a

ed :: TextBuffer tags -> EditText tags IO a -> IO a
ed = testTextEditor error

edln :: Show tags => TextBuffer tags -> EditLine tags IO a -> IO a
edln buf = ed buf . editLine

defaultTags :: Tags
defaultTags = ()

report :: MonadIO m => String -> m ()
report = liftIO . putStr

----------------------------------------------------------------------------------------------------

-- | The most fundamental tests: whether 'insetChar', 'moveByChar', and 'copyCharsRange' do not
-- crash. These tests operate on a line buffer, but the same vector computations used for the line
-- buffer are also used for the full text buffer, so ensuring these functions are correct goes more
-- than half way to ensuring the text editor functions are correct as well. Since other tests make
-- assumptions that the fundamentally functionalty that is tested here is working correctly in order
-- for those other tests to initialize their testing environments, these tests are called
-- "pre-tests."
lineEditorPreTests :: Spec
lineEditorPreTests = describe "line editor pre-tests" $ do
  buf <- runIO $ newTextBuffer defaultTags
  let ins dir ch expct =
        it ("*** insertChar "++show dir++' ':show ch) $
        ed buf (insertChar dir ch >> unpack <$> copyLineEditorText)
        `shouldReturn` expct
  let copy1way dir at len expct =
        it ("*** copyRegion ("++show at++") ("++show len++") -- in "++dir++" direction") $
        edln buf (unpack <$> copyCharsRange at len)
        `shouldReturn` expct
  let copy at len expct = do
        copy1way "forward" at len expct
        copy1way "reverse" (shiftAbsolute at len) (negate len) expct
  let copyEach str n = do
        let len   = length str
        let count = charToCount n
        mapM_ (uncurry $ uncurry copy) $ [1 ..] `zip` repeat n `zip`
          (take count . fst <$> (tails str `zip` [0 .. len - count]))
  let copy123 = forM_ [1..3] . copyEach
  let toEnd dir = editLine $ (,) dir . unpack <$> copyCharsToEnd dir
  let move rel expbef expaft = 
        it ("*** moveByChar ("++show rel++")") $
        ed buf (moveByChar rel >> (,) <$> toEnd Before <*> toEnd After)
        `shouldReturn` ((Before, expbef), (After, expaft))
  ins Before 'A' "A"
  ins After  'F' "AF"
  move (-1) "" "AF"
  move  (2) "AF" ""
  move (-2) "" "AF"
  move  (1) "A" "F"
  ins Before 'B' "ABF"
  ins After  'E' "ABEF"
  move (-2) "" "ABEF"
  move  (3) "ABE" "F"
  move (-3) "" "ABEF"
  move  (4) "ABEF" ""
  move (-2) "AB" "EF"
  ins Before 'C' "ABCEF"
  ins After  'D' "ABCDEF"
  move (-3) "" "ABCDEF"
  copy123 "ABCDEF"
  move  (6) "ABCDEF" ""
  copy123 "ABCDEF"
  move (-6) "" "ABCDEF"
  move  (3) "ABC" "DEF"
  copy123 "ABCDEF"
  move  (2) "ABCDE" "F"
  move (-3) "AB" "CDEF"
  copy123 "ABCDEF"
  move  (4) "ABCDEF" ""
  move (-5) "A" "BCDEF"
  move (-1) "" "ABCDEF"
  move  (1) "A" "BCDEF"
  move  (4) "ABCDE" "F"
  move  (1) "ABCDEF" ""
  move (-1) "ABCDE" "F"
  copy123 "ABCDEF"

-- | These tests setup a line editor in a text buffer, and performs a series of steps. Each step
-- makes some stateful changes to the line editor and checking that the state of the line editor is
-- exactly the value that is to be expected.
lineEditorTests :: Spec
lineEditorTests = describe "line editor, cumultative state update tests" $ 
  it ("*** insertChar, moveByChar, copyLineEditorText, copyCharsRange") $ do
    buf <- newTextBuffer defaultTags
    let instr dir str = ed buf $ forM_ str $ insertChar dir
    instr After $ reverse "characters after"
    instr Before "characters before "
    return buf
    -- [Initial line buffer state]
    -- "characters before characters after"
    --                    ^
    let test rel dir str i count expctPart expctFull =
          ( ed buf $ do
              moveByChar rel
              forM_ (str :: String) (insertChar dir)
              editLine $ (,)
                <$> (unpack <$> copyLineEditorText)
                <*> (unpack <$> copyCharsRange i count)
          ) `shouldReturn` (expctFull, expctPart)
    test minBound Before "what "      17 11
                      "before char"
      "what characters before characters after"
    test maxBound Before " now"       24 20
                             "characters after now"
      "what characters before characters after now"
    test (-20)    Before "the other "  1 26
      "what characters before the"
      "what characters before the other characters after now"

-- | The next simplest tests, tests whether 'insertString' and 'gotoPosition' do not crash, it is
-- not tested whether or not the text inserted is correct. These functions are used during
-- initialization of all other tests, so they at least need be able to run without crashing, whether
-- they run correctly is tested by other tests.
textEditorPreTests :: Spec
textEditorPreTests = describe "text editor pre-tests" $ do
  buf <- runIO $ newTextBuffer defaultTags
  let ins str =
        it ("insertString " ++ show str) $
        ed buf (void $ insertString str)
  let move msg line col = let pos = mkLoc line col in
        it ("move cursor "++msg++", gotoPosition ("++show pos++")") $
        ed buf (void $ gotoPosition $ mkLoc line col)
  ins "one two three\n"
  ins "four five six\nseven eight nine\nten eleven twelve\n"
  move        "up" 1  1
  move      "down" 4 16
  move "to middle" 2  7

----------------------------------------------------------------------------------------------------

-- | A list that is intended to be split at some point. The split is encoded as a tuple, with a list
-- scanned elements are stacked in reversed order in the 'fst' of the tuple and unscanned elements
-- are left in forward order in the 'snd' of the tuple.
type Zipper a = ([a], [a])
type ZipperArrow a = Zipper a -> Zipper a

newStringZipper :: String -> Zipper Char
newStringZipper = (,) ""

zipperToString :: Zipper Char -> String
zipperToString = snd . moveToStart

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

-- | Apply a 'ZipperArrow' in the reverse direction, i.e. flip the 'fst' and 'snd' element of the
-- input, then apply the given 'ZipperArrow', then flip 'fst' and 'snd' of the output.
reverseZipperArrow :: ZipperArrow a -> ZipperArrow a
reverseZipperArrow f = swap . f . swap

-- | Like 'Data.List.break' but on a 'Zipper'.
zBreak :: (a -> Bool) -> ZipperArrow a
zBreak p = uncurry loop where
  loop stack = \ case
    []   -> (stack, [])
    a:ax -> if p a then (stack, a:ax) else loop (a:stack) ax

zNTimes :: Int -> ZipperArrow a -> ZipperArrow a
zNTimes i f = if i <= 0 then id else \ case
  (back, []) -> (back, [])
  st         -> zNTimes (i-1) f $ f st

zStep :: ZipperArrow a
zStep = \ case
  (back, []   ) -> (back, [])
  (back, a:fwd) -> (a:back, fwd)

zSafeTail :: ZipperArrow a
zSafeTail = \ case
  (back, []   ) -> (back, [])
  (back, _:fwd) -> (back, fwd)

-- | Only evaluate the 'ZipperArrow' if the forward stack is not null
zNotNull :: ZipperArrow a -> ZipperArrow a
zNotNull f st = if null $ snd st then st else f st

zSplitAt :: Int -> ZipperArrow a
zSplitAt i = zNTimes i zStep

zDrop :: Int -> ZipperArrow a
zDrop i = zNTimes i zSafeTail

-- | Move all elements from the reverse stack back onto the forward stack.
moveToStart :: ZipperArrow a
moveToStart = \ case
  ([]  , bx) -> ([], bx)
  (a:ax, bx) -> moveToStart (ax, a:bx)

moveToEnd :: ZipperArrow a
moveToEnd = reverseZipperArrow moveToStart

stepOverLineBreak :: ZipperArrow Char
stepOverLineBreak (back, fwd) = case fwd of
  '\r':'\n':fwd -> ('\n' : '\r' : back, fwd)
  '\n':'\r':fwd -> ('\r' : '\n' : back, fwd)
  '\r':fwd      -> ('\r' : back       , fwd)
  '\n':fwd      -> ('\n' : back       , fwd)
  fwd           -> (back, fwd)

removeLineBreak :: ZipperArrow Char
removeLineBreak (back, fwd) = let r = (,) back in case fwd of
  '\r':'\n':fwd -> r fwd
  '\n':'\r':fwd -> r fwd
  '\r':fwd      -> r fwd
  '\n':fwd      -> r fwd
  fwd           -> r fwd

deleteCharUnits :: CharUnitCount -> ZipperArrow Char
deleteCharUnits (CharUnitCount i) = case compare i 0 of
  EQ -> id
  GT -> zNTimes i del
  LT -> reverseZipperArrow $ zNTimes (negate i) del
  where { del = zSafeTail . removeLineBreak; }

-- | Scan forward to the next line.
splitToNextLine :: ZipperArrow Char
splitToNextLine = stepOverLineBreak . zBreak (\ c -> c == '\n' || c == '\r')

splitAtLine :: Absolute LineIndex -> ZipperArrow Char
splitAtLine = loop 0 . lineToIndex where
  loop i targ = if i >= targ then id else zNotNull $ loop (i+1) targ . splitToNextLine

zLines :: String -> [String]
zLines = \ case
  ""  -> []
  str -> let (back, fwd) = splitToNextLine $ newStringZipper str in reverse back : zLines fwd

splitAtLocation :: TextLocation -> ZipperArrow Char
splitAtLocation loc =
  zSplitAt (charToIndex $ theLocationCharIndex loc) .
  splitAtLine (theLocationLineIndex loc)

selectStringRange :: TextLocation -> TextLocation -> Zipper Char -> String
selectStringRange a0 b0 =
  let a = min a0 b0
      b1 = max a0 b0
      line = theLocationLineIndex
      char = theLocationCharIndex
      b = b1{ theLocationLineIndex =
                1 `shiftAbsolute` (line a `diffAbsolute` line b1)
            , theLocationCharIndex = if line a /= line b1 then char b1 else
                1 `shiftAbsolute` (char a `diffAbsolute` char b1)
            }
  in  reverse . fst . splitAtLocation b . newStringZipper . snd . splitAtLocation a

gridLines :: [String]
gridLines = (\ a -> unwords ((\ b -> [a,b]) <$> chars) ++ "\n") <$> chars where
  chars = "0123456789ABCDEF"
{-# INLINE gridLines #-}

grid :: String
grid = concat gridLines
{-# INLINE grid #-}

testWithGrid :: (TextBuffer Tags -> Spec) -> Spec
testWithGrid = (>>=) $ runIO $ do
  buf <- newTextBuffer defaultTags
  ed buf $ insertString grid
  return buf

----------------------------------------------------------------------------------------------------

-- | A wrapper around a list of strings that instantiates the 'Eq' and 'Show' typeclasses so as to
-- produce better output when they are pretty-printed by the Hspec logging function.
data Lines
  = Lines
    { lineTestInfo :: String
    , unwrapLines  :: String
    }
instance Eq Lines where
  a == b = unwrapLines a == unwrapLines b
instance Show Lines where
  show (Lines info str) =
    (if null info then "" else "   " ++ info ++ "\n") ++
    (zLines str >>= (++ "\n") . ("    | " ++) . show)

-- | Tests the 'textView' function, which has some arithmetical computations regarding vector slices
-- with line breaks that are not shared with most other functions, so 'textView' needs it's own
-- extensive set of tests. 'textView' is the function used to inspect the state of the 'TextBuffer',
-- so ANY tests for any functions that can update the 'TextBuffer' depend on this test, because all
-- tests of 'TextBuffer' updating functions require a correct implementation of 'textView' in order
-- to inspect the updates.
textViewTests :: Spec
textViewTests = describe "testing 'textView'" $ testWithGrid $ \ buf -> do
  let vi a b = it ("*** textView ("++showLoc a++") ("++showLoc b++")") $
        ( ed buf $ Lines "" . (>>= fst) . textViewToStrings <$> textView a b
        ) `shouldReturn` Lines "" (selectStringRange a b $ newStringZipper grid)
  describe ("move cursor to start of buffer 1 1") $ do
    vi minBound      maxBound
    vi (mkLoc  3 25) (mkLoc  6 24)
    vi (mkLoc 14  1) (mkLoc 16 48)
    vi (mkLoc  1  1) (mkLoc  3 48)
    vi (mkLoc  1 12) (mkLoc  1 28)
    vi (mkLoc  1 12) (mkLoc  9 28)
  describe ("move cursor to start of buffer 1 1") $ do
    runIO $ ed buf $ gotoPosition $ mkLoc 1 1
    vi (mkLoc  3 24) (mkLoc  5 25)
    vi (mkLoc 14  1) (mkLoc 15 48)
    vi (mkLoc  1  1) (mkLoc  2 48)
    vi (mkLoc  1 12) (mkLoc  1 25)
    vi (mkLoc  3 13) (mkLoc  6 49)
    vi (mkLoc  9 12) (mkLoc  8 25)
  describe ("move cursor to middle of buffer 8 23") $ do
    runIO $ ed buf $ gotoPosition $ mkLoc 8 23
    vi (mkLoc  1  1) (mkLoc  8 48)
    vi (mkLoc  7 24) (mkLoc 11 25)
    vi (mkLoc  7  1) (mkLoc  8 49)
    vi (mkLoc  8  1) (mkLoc  9 48)
    vi (mkLoc  9  1) (mkLoc 10 49)
    vi (mkLoc  1  1) (mkLoc 16 48)
    vi (mkLoc  8  1) (mkLoc  8 49)

----------------------------------------------------------------------------------------------------

cursorMotionTests :: Spec
cursorMotionTests = describe "testing cursor motion" $ testWithGrid $ \ buf -> do
  let go loc =
        let TextLocation{theLocationLineIndex=line,theLocationCharIndex=char} = loc
            expct = splitAt (charToIndex char) $ gridLines !! lineToIndex line
        in it ("*** gotoPosition ("++show loc++")") $
           ( ed buf $ do
               gotoPosition loc
               newloc  <- getPosition
               content <- editLine $ (,)
                 <$> (unpack <$> copyCharsToEnd Before)
                 <*> (unpack <$> copyCharsToEnd After)
               return (newloc, content)
           ) `shouldReturn` (loc, expct)
  go (mkLoc 16 24)
  go (mkLoc 16  8)
  go (mkLoc 16 32)
  go (mkLoc  1  1)
  go (mkLoc  1 48)
  go (mkLoc  1 24)
  go (mkLoc  8 48)
  go (mkLoc  8  1)
  go (mkLoc  8 24)
  go (mkLoc  9 24)
  go (mkLoc  9  1)
  go (mkLoc  9 48)
  go (mkLoc 16 48)

----------------------------------------------------------------------------------------------------

textDeletionTests :: Spec
textDeletionTests = describe "text deletion tests" $ testWithGrid $ \ buf ->
  it "*** deleteCharsWrap tests" $ do
    fakebuf <- newIORef grid
    let del at len = do
          let info = "deleteCharsWrap ("++show at++") ("++show len++")"
          (_grid0, grid1) <- liftIO $ do
            grid0 <- readIORef fakebuf
            modifyIORef fakebuf $
              zipperToString . deleteCharUnits len . splitAtLocation at . newStringZipper
            (,) grid0 <$> readIORef fakebuf
          flip shouldReturn (Lines info grid1) $ ed buf $ do
            gotoPosition at
            deleteCharsWrap len
            flushLineEditor
            Lines info . (>>= fst) . textViewToStrings <$>
              textView (mkLoc minBound minBound) (mkLoc maxBound maxBound)
    del (mkLoc 16 25) (-24)
    del (mkLoc 16  1)  (25)
    del (mkLoc 14  1)  (49)
    del (mkLoc 13 48) (-49)
    del (mkLoc 12 24) (-49)
