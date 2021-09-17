module Domain.Todo where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import qualified Data.Aeson                    as JSON
import           Data.Text
import           Data.Time                      ( UTCTime(..)
                                                , fromGregorian
                                                , secondsToDiffTime
                                                )
import           GHC.Generics
import           Servant.API
import           Utils                          ( toJsonOptions )

data Todo = Todo
  { todoId          :: Integer
  , todoTitle       :: Text
  , todoDescription :: Text
  , todoIsComplete  :: Bool
  , todoCreatedAt   :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON Todo where
  toJSON = JSON.genericToJSON $ toJsonOptions "todo"



dummyTodos :: [Todo]
dummyTodos =
  [ Todo 1
         "Chores"
         "Clean up this mess"
         False
         (UTCTime (fromGregorian 2021 9 17) (secondsToDiffTime 0))
  , Todo 2
         "Haskell"
         "Learn haskell"
         False
         (UTCTime (fromGregorian 2021 9 17) (secondsToDiffTime 0))
  , Todo 3
         "Servant"
         "Learn about servant framework"
         False
         (UTCTime (fromGregorian 2021 9 17) (secondsToDiffTime 0))
  ]
