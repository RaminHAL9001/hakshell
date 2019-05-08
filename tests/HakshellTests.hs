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

ralign :: Int -> String
ralign n = case abs n of
  i | i < 10 -> "   " ++ show i
  i | i < 100 -> "  " ++ show i
  i | i < 1000 -> " " ++ show i
  i             ->       show i

showBuffer :: Show tags => EditText tags IO ()
showBuffer = do
  errCount <- forLinesInBuffer (0 :: Int) $ \ _halt line -> do
    when (textLineIsUndefined line) $ modify (+ 1)
    (Absolute (LineIndex lineNum)) <- currentLineNumber
    liftIO $ putStrLn $ ralign lineNum ++ ": " ++ show line
    return [line]
  liftIO $ putStrLn ""
  when (errCount > 0) $ error $ "iterated over "++show errCount++" undefined lines"

showView :: (MonadIO m, Show tags) => TextView tags -> m ()
showView view = do
  (_, errCount) <- forLinesInView view (1 :: Int, 0 :: Int) $ \ _halt line -> do
    lineNum <- state $ \ (lineNum, errCount) ->
      (lineNum, (lineNum + 1, errCount + if textLineIsUndefined line then 1 else 0))
    liftIO $ putStrLn $ ralign lineNum ++ ": " ++ show line
  liftIO $ putStrLn ""
  when (errCount > 0) $ error $ "iterated over "++show errCount++" undefined lines"

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
      gotoCursor (mkLoc 1  1)
      showBuffer
      report "Move cursor down...\n"
      gotoCursor (mkLoc 4 16)
      showBuffer
      report "Move cursor to middle...\n"
      gotoCursor (mkLoc 2  7)
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
  reportView (mkLoc  3 24) (mkLoc  6 25)
  reportView (mkLoc 14  1) (mkLoc 16 48)
  reportView (mkLoc  1  1) (mkLoc  3 48)
  reportView (mkLoc  1 12) (mkLoc  1 28)
  reportView (mkLoc  1 12) (mkLoc  9 28)
  report "\nMove cursor to start of buffer..."
  testTextEditor error buf $ gotoPosition $ mkLoc 0 0
  report "OK\n"
  reportView (mkLoc  3 24) (mkLoc  5 25)
  reportView (mkLoc 14  1) (mkLoc 15 48)
  reportView (mkLoc  1  1) (mkLoc  2 48)
  reportView (mkLoc  1 12) (mkLoc  0 25)
  reportView (mkLoc  9 12) (mkLoc  8 25)
  report "\nMove cursor to middle of buffer..."
  testTextEditor error buf $ gotoPosition $ mkLoc 8 23
  report "OK\n"
  reportView (mkLoc  1  1) (mkLoc  8 48)
  reportView (mkLoc  7 24) (mkLoc 11 25)
  reportView (mkLoc  7  1) (mkLoc  8 48)
  reportView (mkLoc  8  1) (mkLoc  9 48)
  reportView (mkLoc  9  1) (mkLoc 10 48)
  reportView (mkLoc  1  1) (mkLoc 16 48)
