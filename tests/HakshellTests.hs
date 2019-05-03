module Main where

import           Hakshell.String
import           Hakshell.TextEditor

import           Control.Monad.IO.Class

----------------------------------------------------------------------------------------------------

main :: IO ()
main = begin >> newTextBuffer defaultTags >>= runEditText editTextTests >>= \ case
  Left (TextEditError err) -> putStrLn $ unpack err
  Right () -> return ()

begin :: IO ()
begin = putStrLn "\n-------------------------\nBegin HakshellTest Log\n-------------------------\n"

----------------------------------------------------------------------------------------------------

type Tags = ()

defaultTags :: Tags
defaultTags = ()

report :: MonadIO m => String -> m ()
report = liftIO . putStr

editTextTests :: EditText Tags ()
editTextTests = do
  let reportInsert str = do
        report $ "insertString " ++ show str ++ "\n"
        insertString str
  reportInsert "one two three\n"
  reportInsert "four five six\nseven eight nine\nten eleven twelve\n"
  report "OK\n"
  forLinesInBuffer (1::Int) $ \ _ line -> do
    n <- state $ \ n -> (n, n+1)
    liftIO $ putStrLn $ show n ++ ": " ++ unpack line
    return [line]
  return ()
