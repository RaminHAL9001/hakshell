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

testTextEditor :: EditText tags IO () -> TextBuffer tags -> IO ()
testTextEditor f = runEditTextIO f >=> \ case
  Left (TextEditError err) -> putStrLn $ unpack err
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
basicTests = newTextBuffer defaultTags >>= testTextEditor
  (do report "--- basic tests ---\n"
      let reportInsert str = do
            report $ "insertString " ++ show str ++ "\n"
            insertString str
      reportInsert "one two three\n"
      reportInsert "four five six\nseven eight nine\nten eleven twelve\n"
      showBuffer
      report "OK\n"
  )

textViewTests :: IO ()
textViewTests = do
  report "--- text view tests ---\n"
  buf <- newTextBuffer defaultTags
  report "fill buffer...\n"
  flip testTextEditor buf $ do
    let chars = "0123456789ABCDEF"
    insertString $ chars >>= \ a -> unwords ((\ b -> [a,b]) <$> chars) ++ "\n"
    showBuffer
  let reportView a b = do
        report $ "view "++showLoc a++"->"++showLoc b++"\n"
        v <- textView a b buf
        showView v
  reportView (mkLoc 2 23) (mkLoc 5 24)
