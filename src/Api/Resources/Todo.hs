module Api.Resources.Todo where

import           Data.Aeson                     ( (.:)
                                                , FromJSON(..)
                                                , ToJSON(..)
                                                )
import qualified Data.Aeson                    as JSON
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Data.UUID                      ( UUID )
import           Domain.Todo                    ( Todo(..) )
import           GHC.Generics                   ( Generic )
import           Utils                          ( jsonOptions )

data NewTodoRequestDto = NewTodoRequestDto
  { newTodoReqTitle       :: Text
  , newTodoReqDescription :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON NewTodoRequestDto where
  parseJSON = JSON.genericParseJSON $ jsonOptions "reqTodo"

data UpdateTodoRequestDto = UpdateTodoRequestDto
  { updateTodoReqTitle       :: Text
  , updateTodoReqDescription :: Text
  , updateTodoReqIsComplete  :: Bool
  }
  deriving (Eq, Show, Generic)

instance FromJSON UpdateTodoRequestDto where
  parseJSON = JSON.genericParseJSON $ jsonOptions "reqTodo"

data TodoResponseDto = TodoResponseDto
  { resTodoId            :: UUID
  , resTodoTitle         :: Text
  , resTodoDescription   :: Text
  , resTodoIsComplete    :: Bool
  , resTodoCreatedAt     :: UTCTime
  , resTodoLastUpdatedAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON TodoResponseDto where
  toJSON = JSON.genericToJSON $ jsonOptions "resTodo"

toResponseDto :: Todo -> TodoResponseDto
toResponseDto (Todo id title description isComplete createdAt updatedAt) =
  TodoResponseDto id title description isComplete createdAt updatedAt
