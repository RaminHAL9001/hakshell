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
        ed buf (gotoPosition $ mkLoc line col)
  ins "one two three\n"
  ins "four five six\nseven eight nine\nten eleven twelve\n"
  move        "up" 1  1
  move      "down" 4 16
  move "to middle" 2  7

----------------------------------------------------------------------------------------------------

selectStrings :: TextLocation -> TextLocation -> [String] -> [String]
selectStrings a b lines =
  let ( TextLocation{theLocationLineIndex=lineA0,theLocationCharIndex=charA0},
        TextLocation{theLocationLineIndex=lineB0,theLocationCharIndex=charB0}
       ) = (min a b, max a b)
      lineA = lineToIndex lineA0
      lineB = lineToIndex lineB0
      charA = charToIndex charA0
      charB = charToIndex charB0
      final = \ case
        []         -> []
        [line]     -> [take charB line]
        line:lines -> line : final lines
  in  case take (lineB - lineA + 1) $ drop (max 0 lineA) $ lines of
        []         -> []
        [line]     -> let (lo, hi) = (min charA charB, max charA charB) in
                        [take (hi - lo) $ drop lo line]
        line:lines -> drop charA line : final lines

gotoListItem :: Int -> [a] -> ([a] -> [a] -> a -> b) -> [a] -> b
gotoListItem i rev f = \ case
  []    -> error $ "bug in test code: (gotoListItem "++show i++"), index is out of bounds"
  a:fwd -> if i <= 0 then f rev fwd a else (gotoListItem $! i - 1) (a : rev) f fwd

dropChars :: Int -> [String] -> [String]
dropChars i = if i <= 0 then id else \ case
  []               -> []
  "":lines         -> dropChars i lines -- <-- DO NOT subtract 1 from i here.
  (_c:chars):lines -> (dropChars $! i - 1) (chars : lines)

deleteStrings :: TextLocation -> Relative CharIndex -> [String] -> [String]
deleteStrings (TextLocation{theLocationLineIndex=line,theLocationCharIndex=char}) len =
  let count = charToCount len in
  if count == 0 then id else
    gotoListItem (lineToIndex line) [] $ \ revLines fwdLines ->
    let chidx = charToIndex char in
    gotoListItem chidx "" $ \ revChars fwdChars _c -> -- <-- drop _c, must subtract 1 from count
    if abs count <= chidx then
      if count < 0 then
        reverse (reverse (drop (negate count - 1) revChars :: String) : revLines :: [String]) ++ (fwdChars :: String) : fwdLines :: [String]
      else
        reverse ((reverse revChars :: String) : revLines :: [String]) ++ (drop (count - 1) fwdChars :: String) : fwdLines :: [String]
    else if count < 0 then
      reverse (dropChars (negate $ count + chidx + 1) revLines :: [String]) ++ (fwdChars :: String) : fwdLines :: [String]
    else
      reverse ((reverse revChars :: String) : revLines :: [String]) ++ (dropChars (count - chidx - 1) fwdLines :: [String]) :: [String]

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
    , unwrapLines  :: [String]
    }
instance Eq Lines where
  a == b = unwrapLines a == unwrapLines b
instance Show Lines where
  show (Lines info lines) =
    (if null info then "" else "   " ++ info ++ "\n") ++
    (lines >>= (++ "\n") . ("    | " ++) . show)

-- | Tests the 'textView' function, which has some arithmetical computations regarding vector slices
-- with line breaks that are not shared with most other functions, so 'textView' needs it's own
-- extensive set of tests. 'textView' is the function used to inspect the state of the 'TextBuffer',
-- so ANY tests for any functions that can update the 'TextBuffer' depend on this test, because all
-- tests of 'TextBuffer' updating functions require a correct implementation of 'textView' in order
-- to inspect the updates.
textViewTests :: Spec
textViewTests = describe "testing 'textView'" $ testWithGrid $ \ buf -> do
  let vi a b = it ("*** textView ("++showLoc a++") ("++showLoc b++")") $
        ( ed buf $ Lines "" . fmap fst . textViewToStrings <$> textView a b
        ) `shouldReturn` Lines "" (selectStrings a b gridLines)
  describe ("move cursor to start of buffer 1 1") $ do
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
           (ed buf $ gotoPosition loc >>
             (,) <$> getPosition <*> editLine
             ((,) <$> (unpack <$> copyCharsToEnd Before) <*> (unpack <$> copyCharsToEnd After))
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
    fakebuf <- newIORef gridLines
    let del at len = do
          let info = "deleteCharsWrap ("++show at++") ("++show len++")"
          grid0 <- liftIO $ readIORef fakebuf
          let grid1 = deleteStrings at len grid0
          liftIO $ writeIORef fakebuf grid1
          flip shouldReturn (Lines info grid1) $ ed buf $ do
            gotoPosition at
            deleteCharsWrap len
            Lines info . fmap fst . textViewToStrings <$>
              textView (mkLoc minBound minBound) (mkLoc maxBound maxBound)
    del (mkLoc 16 24) (-24)
    del (mkLoc 16  1)  (24)
    del (mkLoc 14  1)  (49)
    del (mkLoc 13 48) (-49)
    del (mkLoc 12 24) (-49)
