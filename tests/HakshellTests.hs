module Main where

import           Hakshell.String
import           Hakshell.TextEditor

import           Data.List (tails)

import           Control.Monad.IO.Class
import           Control.Monad.Reader

import           Test.Hspec

----------------------------------------------------------------------------------------------------

main :: IO ()
main = hspec $ describe "hakshell" $ do
  lineEditorTests
  textEditorTests
  moveCursorTests
  textViewTests

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

-- | The simplest tests, tests whether 'insetChar' and 'moveByChar' do not crash, it is not tested
-- whether or not the characters inserted are correct. These functions are used during
-- initialization of all other tests, so they at least need be able to run without crashing, whether
-- they run correctly is tested by other tests.
lineEditorTests :: Spec
lineEditorTests = describe "EditLine" $ do
  buf <- runIO $ newTextBuffer defaultTags
  let instr dir str =
        it ("insertChar "++show dir++' ':show str) $
        ed buf (forM_ str (insertChar dir))
  instr After $ reverse "characters after"
  instr Before "characters before "
  let move rel = it ("moveByChar ("++show rel++")") $
        ed buf (moveByChar rel)
  let select i count expct =
        it ("copyCharsRange "++show i++' ':show count++" ---\n") $
        edln buf (unpack <$> copyCharsRange i count)
        `shouldReturn` expct
  select 12 11 "before char"
  move minBound
  instr Before "what "
  move maxBound
  instr Before " now"
  move (-20)
  instr Before "the what now"

-- | The next simplest tests, tests whether 'insertString' and 'gotoPosition' do not crash, it is
-- not tested whether or not the text inserted is correct. These functions are used during
-- initialization of all other tests, so they at least need be able to run without crashing, whether
-- they run correctly is tested by other tests.
textEditorTests :: Spec
textEditorTests = describe "EditText" $ do
  buf <- runIO $ newTextBuffer defaultTags
  let ins str =
        it ("insertString " ++ show str) $
        ed buf (void $ insertString str)
        -- TODO: show the content of the whole buffer as a string here.
  let move msg line col = let pos = mkLoc line col in
        it ("move cursor "++msg++", gotoPosition ("++show pos++")") $
        ed buf (gotoPosition $ mkLoc line col)
        -- TODO: show the content of the whole buffer as a string here.
  let del msg n =
        it ("delete on "++msg++", deleteCharsWrap "++show n) $
        ed buf (void $ deleteCharsWrap $ Relative $ CharIndex n)
        -- TODO: show the content of the whole buffer as a string here.
  ins "one two three\n"
  ins "four five six\nseven eight nine\nten eleven twelve\n"
  move        "up" 1  1
  move      "down" 4 16
  move "to middle" 2  7
  del "same line" (-4)
  del "to \"three\" on previous line" (-8)
  return ()

-- | This function tests whether 'insertChar', 'moveByChar', and 'copyCharsRange' produce correct
-- results. These tests operate on a line buffer, but the same vector computations used for the line
-- buffer are also used for the full text buffer, so ensuring these functions are correct goes more
-- than half way to ensuring the text editor functions are correct as well.
moveCursorTests :: Spec
moveCursorTests = describe "moveCursor" $ do
  buf <- runIO $ newTextBuffer defaultTags
  let ins dir ch expct =
        it ("insertChar "++show dir++' ':show ch) $
        ed buf (insertChar dir ch >> unpack <$> copyLineEditorText)
        `shouldReturn` expct
  let copy1way dir at len expct =
        it ("copyRegion ("++show at++") ("++show len++") -- in "++dir++" direction") $
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
  let toEnd dir = (,) dir . unpack <$> copyCharsToEnd dir
  let move rel expbef expaft = 
        it ("moveByChar ("++show rel++")") $
        edln buf (moveByChar rel >> (,) <$> toEnd Before <*> toEnd After)
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

----------------------------------------------------------------------------------------------------

newtype Lines = Lines{ unwrapLines :: [String] } deriving (Eq, Ord)
instance Show Lines where
  show (Lines lines) = "\n" ++ (lines >>= (++ "\n") . ("    " ++) . show)

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

-- | Tests the 'textView' function, which has some arithmetical computations regarding vector slices
-- with line breaks that are not shared with most other functions, so 'textView' needs it's own
-- extensive set of tests.
textViewTests :: Spec
textViewTests = describe "TextView" $ do
  let chars     = "0123456789ABCDEF"
  let gridLines = (\ a -> unwords ((\ b -> [a,b]) <$> chars) ++ "\n") <$> chars :: [String]
  let grid      = concat gridLines :: String
  buf <- runIO $ do
    buf <- newTextBuffer defaultTags
    ed buf $ insertString grid
    return buf
  let vi a b = it ("textView ("++showLoc a++") ("++showLoc b++")") $
        ( textView a b buf >>= \ case
            Left err -> error $ show err
            Right  v -> return $ Lines $ fst <$> textViewToStrings v
        ) `shouldReturn` Lines (selectStrings a b gridLines)
  it ("move cursor to start of buffer: gotoPosition 1 1") $
    ed buf $ gotoPosition (mkLoc 1 1)
  vi (mkLoc  3 25) (mkLoc  6 24)
  vi (mkLoc 14  1) (mkLoc 16 48)
  vi (mkLoc  1  1) (mkLoc  3 48)
  vi (mkLoc  1 12) (mkLoc  1 28)
  vi (mkLoc  1 12) (mkLoc  9 28)
  it ("move cursor to start of buffer: gotoPosition 1 1") $
    ed buf $ gotoPosition $ mkLoc 1 1
  vi (mkLoc  3 24) (mkLoc  5 25)
  vi (mkLoc 14  1) (mkLoc 15 48)
  vi (mkLoc  1  1) (mkLoc  2 48)
  vi (mkLoc  1 12) (mkLoc  1 25)
  vi (mkLoc  9 12) (mkLoc  8 25)
  it ("move cursor to middle of buffer: gotoPosition 8 23") $
    ed buf $ gotoPosition $ mkLoc 8 23
  vi (mkLoc  1  1) (mkLoc  8 48)
  vi (mkLoc  7 24) (mkLoc 11 25)
  vi (mkLoc  7  1) (mkLoc  8 49)
  vi (mkLoc  8  1) (mkLoc  9 48)
  vi (mkLoc  9  1) (mkLoc 10 49)
  vi (mkLoc  1  1) (mkLoc 16 48)
  vi (mkLoc  8  1) (mkLoc  8 49)
  return ()
