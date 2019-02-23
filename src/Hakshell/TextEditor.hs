module Hakshell.TextEditor
  ( EditText, runEditText,
  , module Hakshell.String
  ) where

import           Hakshell.String

import           Control.Lens
import           Control.Monad.State

import qualified Data.ByteString       as Strict
import qualified Data.ByteString.UTF8  as UTF8
import           Data.Sequence

----------------------------------------------------------------------------------------------------
