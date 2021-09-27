module Domain.Exceptions where

import           Control.Exception              ( Exception )
import           Data.Text                      ( Text )


newtype MyException = MyException Text deriving (Eq, Show)

instance Exception MyException
