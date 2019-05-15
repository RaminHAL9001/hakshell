module Main where

import           Hakshell.String
import           Hakshell.TextEditor

import           Control.Monad.IO.Class

----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  begin
  basicTests
  textViewTests

begin :: IO ()
begin = putStrLn "\n-------------------------\nBegin HakshellTest Log\n-------------------------\n"

----------------------------------------------------------------------------------------------------

type Tags = ()

mkLoc :: Int -> Int -> TextLocation
mkLoc a b = TextLocation
  { theCursorLineIndex = Absolute $ LineIndex a
  , theCursorCharIndex = Absolute $ CharIndex b
  }

showLoc :: TextLocation -> String
showLoc
  (TextLocation
   {theCursorLineIndex=Absolute(LineIndex line)
   ,theCursorCharIndex=Absolute (CharIndex char)
   }) = '(' : show line ++ ':' : show char ++ ")"

testTextEditor :: (String -> IO ()) -> TextBuffer tags -> EditText tags IO () -> IO ()
testTextEditor onErr buf f = runEditTextIO f buf >>= \ case
  Left (TextEditError err) -> onErr $ unpack err
  Right a -> return a

defaultTags :: Tags
defaultTags = ()

report :: MonadIO m => String -> m ()
report = liftIO . putStr

basicTests :: IO ()
basicTests = do
  buf <- newTextBuffer defaultTags
  report "--- basic tests ---\n"
  let reportInsert str = testTextEditor error buf $ do
        report $ "insertString " ++ show str ++ "\n"
        insertString str
        debugPrintBuffer
  let reportMove msg line col = testTextEditor error buf $ do
        report $ "Move cursor "++msg++", line="++show line++" col="++show col++" ...\n"
        gotoCursor $ mkLoc line col
        debugPrintBuffer
  reportInsert "one two three\n"
  reportInsert "four five six\nseven eight nine\nten eleven twelve\n"
  reportMove        "up" 1  1
  reportMove      "down" 4 16
  reportMove "to middle" 2  7
  report "OK\n"

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
        v <- textView a b buf
        debugPrintView v
  reportView (mkLoc  3 24) (mkLoc  6 25)
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
