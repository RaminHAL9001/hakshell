module Main where

import           Hakshell.TextEditor

import           Data.Char

import           Control.Monad.IO.Class

----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  begin
  --moveCursorTests
  --lineEditorTests
  textViewTests
  textEditorTests

begin :: IO ()
begin = putStrLn "\n-------------------------\nBegin HakshellTest Log\n-------------------------\n"

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

testTextEditor :: (String -> IO ()) -> TextBuffer tags -> EditText tags IO () -> IO ()
testTextEditor onErr buf f = runEditTextIO f buf >>= \ case
  Left err -> onErr $ show err
  Right a  -> return a

defaultTags :: Tags
defaultTags = ()

report :: MonadIO m => String -> m ()
report = liftIO . putStr

----------------------------------------------------------------------------------------------------

-- The 'copyRegion' function is a sort of dependency for both the 'lineEditorTests' and the
-- 'textViewTests'.
moveCursorTests :: IO ()
moveCursorTests = do
  buf <- newTextBuffer defaultTags
  testTextEditor error buf $ do
    let ins dir ch = do
          report $ "--- insertChar "++show dir++' ':show ch++"\n"
          insertChar dir ch
    let check expct = do
          txt <- copyLineEditorText
          if unpack txt == expct
           then do
            report "--- content of buffer: "
            liftIO $ print txt
           else error $ "\n  Expecting: "++show expct++"\n  Contents: "++show txt
    let move dir expbef expaft = liftEditLine $ do
          report $ "--- moveByChar ("++show dir++")\n"
          moveByChar dir
          check (expbef++expaft)
          let checkpart what txt exp cont = if unpack txt /= exp
                then error $
                  " characters "++what++
                  " cursor do not match expected value:\n  Expecting: "++show expbef++
                  "\n  Contents: "++show txt
                else cont
          beftxt <- copyCharsToEnd Before
          afttxt <- copyCharsToEnd After
          checkpart "before" beftxt expbef $ checkpart "after" afttxt expaft $ return ()
    ins Before 'A'
    ins After  'F'
    check "AF"
    move (-1) "" "AF"
    move  (2) "AF" ""
    move (-2) "" "AF"
    move  (1) "A" "F"
    ins Before 'B'
    check "ABF"
    ins After  'E'
    check "ABEF"
    move (-2) "" "ABEF"
    move  (3) "ABE" "F"
    move (-3) "" "ABEF"
    move  (4) "ABEF" ""
    move (-2) "AB" "EF"
    ins Before 'C'
    check "ABCEF"
    ins After  'D'
    check "ABCDEF"
    move (-3) "" "ABCDEF"
    move  (6) "ABCDEF" ""
    move (-6) "" "ABCDEF"
    move  (3) "ABC" "DEF"
    move  (2) "ABCDE" "F"
    move (-3) "AB" "CDEF"
    move  (4) "ABCDEF" ""
    move (-5) "A" "BCDEF"
    move (-1) "" "ABCDEF"
    move  (1) "A" "BCDEF"
    move  (4) "ABCDE" "F"
    move  (1) "ABCDEF" ""
    move (-1) "ABCDE" "F"

lineEditorTests :: IO ()
lineEditorTests = do
  report "--- line editor tests ---\n"
  buf <- newTextBuffer defaultTags
  let instr dir str = testTextEditor error buf $ do
        report $ "--- insert string "++
          ((\ (c:cx) -> toLower c : cx) $ show dir)++" cursor: "++show str++" ---\n"
        mapM_ (insertChar dir) str
        copyLineEditorText >>= liftIO . print
  instr After $ reverse "characters after"
  instr Before "characters before "
  let move dir = testTextEditor error buf $ do
        report $ "--- moveByChar "++show dir++" ---\n"
        moveByChar dir
        copyLineEditorText >>= liftIO . print
  let select i count = testTextEditor error buf $ do
        report $ "--- copyCharsRange "++show i++' ':show count++" ---\n"
        editLine (copyCharsRange i count) >>= liftIO . print
  select 12 11
  move minBound
  instr Before "what "
  move maxBound
  instr Before " now"
  move (-20)
  instr Before "the "

textViewTests :: IO ()
textViewTests = do
  report "--- text view tests ---\n"
  buf <- newTextBuffer defaultTags
  report "fill buffer...\n"
  testTextEditor error buf $ do
    let chars = "0123456789ABCDEF"
    insertString $ chars >>= \ a -> unwords ((\ b -> [a,b]) <$> chars) ++ "\n"
    debugPrintBuffer
  testTextEditor error buf $ do
    gotoCursor $ mkLoc 1 1
    debugPrintBuffer
  let reportView a b = do
        report $ "view "++showLoc a++"->"++showLoc b++"\n"
        textView a b buf >>= \ case
          Left err -> error $ show err
          Right  v -> debugPrintView v
  reportView (mkLoc  3 25) (mkLoc  6 24)
  reportView (mkLoc 14  1) (mkLoc 16 48)
  reportView (mkLoc  1  1) (mkLoc  3 48)
  reportView (mkLoc  1 12) (mkLoc  1 28)
  reportView (mkLoc  1 12) (mkLoc  9 28)
  report "Move cursor to start of buffer...\n"
  testTextEditor error buf $ do
    gotoPosition $ mkLoc 1 1
    debugPrintBuffer
  report "OK\n"
  reportView (mkLoc  3 24) (mkLoc  5 25)
  reportView (mkLoc 14  1) (mkLoc 15 48)
  reportView (mkLoc  1  1) (mkLoc  2 48)
  reportView (mkLoc  1 12) (mkLoc  0 25)
  reportView (mkLoc  9 12) (mkLoc  8 25)
  report "Move cursor to middle of buffer...\n"
  testTextEditor error buf $ do
    gotoPosition $ mkLoc 8 23
    debugPrintBuffer
  report "OK\n"
  reportView (mkLoc  1  1) (mkLoc  8 48)
  reportView (mkLoc  7 24) (mkLoc 11 25)
  reportView (mkLoc  7  1) (mkLoc  8 48)
  reportView (mkLoc  8  1) (mkLoc  9 48)
  reportView (mkLoc  9  1) (mkLoc 10 48)
  reportView (mkLoc  1  1) (mkLoc 16 48)
  reportView (mkLoc  8  1) (mkLoc  8 48)

textEditorTests :: IO ()
textEditorTests = do
  buf <- newTextBuffer defaultTags
  report "--- basic tests ---\n"
  let reportInsert str = testTextEditor error buf $ do
        report $ "insertString " ++ show str ++ "\n"
        insertString str
        -- TODO: show the content of the whole buffer as a string here.
        debugPrintBuffer
  let reportMove msg line col = testTextEditor error buf $ do
        report $ "Move cursor "++msg++", line="++show line++" col="++show col++" ...\n"
        gotoCursor $ mkLoc line col
        -- TODO: show the content of the whole buffer as a string here.
        debugPrintBuffer
  let reportDelete msg n = testTextEditor error buf $ do
        report $ "Delete on "++msg++", "++show n++" characters...\n"
        deleteCharsWrap $ Relative $ CharIndex n
        -- TODO: show the content of the whole buffer as a string here.
        debugPrintBuffer
  reportInsert "one two three\n"
  reportInsert "four five six\nseven eight nine\nten eleven twelve\n"
  reportMove        "up" 1  1
  reportMove      "down" 4 16
  reportMove "to middle" 2  7
  reportDelete "same line" (-4)
  reportDelete "to \"three\" on previous line" (-8)
  report "OK\n"
