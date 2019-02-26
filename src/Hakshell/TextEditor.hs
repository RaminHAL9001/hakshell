module Hakshell.TextEditor
  ( EditText, TextBuffer, TextEditError(..), runEditText,
    module Hakshell.String
  ) where

import           Hakshell.String

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State

--import qualified Data.ByteString       as Strict
--import qualified Data.ByteString.UTF8  as UTF8
import           Data.Sequence

----------------------------------------------------------------------------------------------------

newtype EditText a
  = EditText{ unwrapEditText :: ExceptT TextEditError (State TextBuffer) a }
  deriving (Functor, Applicative, Monad)

instance MonadState TextBuffer EditText where { state = EditText . lift . state; }

instance MonadError TextEditError EditText where
  throwError = EditText . throwError
  catchError (EditText try) catch = EditText $ catchError try $ unwrapEditText . catch

data TextBuffer
  = TextBuffer
    { theTextCursorCharCount :: Int
      -- ^ The number of characters before the cursor position in the file.
    , theTextLinesAbove :: Seq StrictBytes
      -- ^ Lines above the cursor
    , theTextLineBefore :: String
      -- ^ The characters before the cursor on the line the cursor is currently on
    , theTextLineAfter  :: String
      -- ^ The characters after the cursor on the line the cursor is currently on
    , theTextLinesBelow :: Seq StrictBytes
      -- ^ Lines below the cursor
    }

data TextEditError
  = TextEditError StrictBytes
  deriving (Eq, Ord)

textCursorCharCount :: Lens' TextBuffer Int
textCursorCharCount = lens theTextCursorCharCount $ \ a b -> a{ theTextCursorCharCount = b }

textLinesAbove :: Lens' TextBuffer (Seq StrictBytes)
textLinesAbove = lens theTextLinesAbove $ \ a b -> a{ theTextLinesAbove = b }

textLineBefore :: Lens' TextBuffer String
textLineBefore = lens theTextLineBefore $ \ a b -> a{ theTextLineBefore = b }

textLineAfter :: Lens' TextBuffer String
textLineAfter = lens theTextLineAfter $ \ a b -> a{ theTextLineAfter = b }

textLinesBelow :: Lens' TextBuffer (Seq StrictBytes)
textLinesBelow = lens theTextLinesBelow $ \ a b -> a{ theTextLinesBelow = b }

runEditText :: EditText a -> TextBuffer -> (Either TextEditError a, TextBuffer)
runEditText (EditText f) = runState $ runExceptT f
