module Domain.User where

import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.UUID                      ( UUID )
import           Domain.Entity                  ( Entity(..) )
import           Domain.Role                    ( Role(..) )
import           GHC.Generics                   ( Generic )
import           Utils                          ( getUuid )

data User = User
  { userId            :: UUID
  , userName          :: Text
  , userLastName      :: Text
  , userEmail         :: Text
  , userRole          :: Role
  , userCreatedAt     :: UTCTime
  , userLastUpdatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq User where
  User { userId = id } == User { userId = id' } = id == id'

instance Entity User where
  identifier User { userId = id } = id

createUser :: Text -> Text -> Text -> Role -> IO User
createUser name lastName email role = do
  uuid <- getUuid
  now  <- getCurrentTime
  return $ User uuid name lastName email role now now
