module May.Errors (ServerError(..)) where

import qualified Control.Exception.Base        as Exception
import qualified Data.Text                     as Text
import  Data.Text                     ( Text )

data ServerError = ServerError Text

instance Exception.Exception ServerError where
  displayException _ = "This should never occur"

instance Show ServerError where
  show (ServerError err) = "Server Error: " ++ Text.unpack err
