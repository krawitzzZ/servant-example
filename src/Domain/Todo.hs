module Domain.Todo where

import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.UUID                      ( UUID )
import           Domain.Entity                  ( Entity(..) )
import           GHC.Generics                   ( Generic )
import           Utils                          ( getUuid )


data Todo = Todo
  { todoId            :: UUID
  , todoTitle         :: Text
  , todoDescription   :: Text
  , todoIsComplete    :: Bool
  , todoCreatedAt     :: UTCTime
  , todoLastUpdatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq Todo where
  Todo { todoId = id } == Todo { todoId = id' } = id == id'

instance Entity Todo where
  identifier Todo { todoId = id } = id

createTodo :: Text -> Text -> IO Todo
createTodo title desc = do
  uuid <- getUuid
  now  <- getCurrentTime
  return $ Todo uuid title desc False now now

complete :: Todo -> IO Todo
complete (Todo id title desc _ created _) =
  Todo id title desc True created <$> getCurrentTime

uncomplete :: Todo -> IO Todo
uncomplete (Todo id title desc _ created _) =
  Todo id title desc False created <$> getCurrentTime

rename :: Text -> Todo -> IO Todo
rename title (Todo id _ desc completed created _) =
  Todo id title desc completed created <$> getCurrentTime

describe :: Text -> Todo -> IO Todo
describe desc (Todo id title _ completed created _) =
  Todo id title desc completed created <$> getCurrentTime
