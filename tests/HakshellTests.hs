module Main where

import           Hakshell.String
import           Hakshell.TextEditor

import           Control.Monad
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

countLine :: MonadState Int m => m Int
countLine = state $ \ n -> (n, n + 1)

showBuffer :: EditText tags IO ()
showBuffer = void $ forLinesInBuffer (1 :: Int) $ \ _halt line -> do
  n <- countLine
  liftIO $ putStrLn $ show n ++ ": " ++ unpack line
  return [line]

showView :: MonadIO m => TextView tags -> m ()
showView view = void $ forLinesInView view (1 :: Int) $ \ _halt line -> do
  n <- countLine
  liftIO $ putStrLn $ show n ++ ": " ++ unpack line

basicTests :: IO ()
basicTests = newTextBuffer defaultTags >>= flip (testTextEditor error)
  (do report "--- basic tests ---\n"
      let reportInsert str = do
            report $ "insertString " ++ show str ++ "\n"
            insertString str
      reportInsert "one two three\n"
      reportInsert "four five six\nseven eight nine\nten eleven twelve\n"
      showBuffer
      report "Move cursor up...\n"
      gotoCursor TextLocation
        { theCursorLineIndex = Absolute $ LineIndex 0
        , theCursorCharIndex = Absolute $ CharIndex 0
        }
      showBuffer
      report "Move cursor down...\n"
      gotoCursor TextLocation
        { theCursorLineIndex = Absolute $ LineIndex 3
        , theCursorCharIndex = Absolute $ CharIndex 16
        }
      showBuffer
      report "Move cursor to middle...\n"
      gotoCursor TextLocation
        { theCursorLineIndex = Absolute $ LineIndex 1
        , theCursorCharIndex = Absolute $ CharIndex 6
        }
      showBuffer
      report "OK\n"
  )

textViewTests :: IO ()
textViewTests = do
  report "--- text view tests ---\n"
  buf <- newTextBuffer defaultTags
  report "fill buffer...\n"
  testTextEditor error buf $ do
    let chars = "0123456789ABCDEF"
    insertString $ chars >>= \ a -> unwords ((\ b -> [a,b]) <$> chars) ++ "\n"
    showBuffer
  let reportView a b = do
        report $ "view "++showLoc a++"->"++showLoc b++"\n"
        v <- textView a b buf
        showView v
  reportView (mkLoc  2 23) (mkLoc  5 24)
  reportView (mkLoc 13  0) (mkLoc 15 47)
  reportView (mkLoc  0  0) (mkLoc  2 47)
  reportView (mkLoc  0 11) (mkLoc  0 24)
  reportView (mkLoc  8 11) (mkLoc  8 24)
  report "\nMove cursor to start of buffer..."
  testTextEditor error buf $ gotoPosition $ mkLoc 0 0
  report "OK\n"
  reportView (mkLoc  2 23) (mkLoc  5 24)
  reportView (mkLoc 13  0) (mkLoc 15 47)
  reportView (mkLoc  0  0) (mkLoc  2 47)
  reportView (mkLoc  0 11) (mkLoc  0 24)
  reportView (mkLoc  8 11) (mkLoc  8 24)
  report "\nMove cursor to middle of buffer..."
  testTextEditor error buf $ gotoPosition $ mkLoc 8 23
  report "OK\n"
  reportView (mkLoc  6 23) (mkLoc 10 24)
  reportView (mkLoc  6  0) (mkLoc  7 47)
  reportView (mkLoc  7  0) (mkLoc  8 47)
  reportView (mkLoc  8  0) (mkLoc  9 47)
