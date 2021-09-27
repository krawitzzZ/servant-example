module Infrastructure.Entities.User where

import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.UUID                      ( UUID )
import           Data.UUID.V4                   ( nextRandom )
import           Database.Beam                  ( Beamable
                                                , Columnar
                                                , Identity
                                                , Table(..)
                                                )
import           Domain.Role                    ( Role(..) )
import           GHC.Generics                   ( Generic )


data UserT f = User
  { userId            :: Columnar f UUID
  , userName          :: Columnar f Text
  , userLastName      :: Columnar f Text
  , userEmail         :: Columnar f Text
  , userRole          :: Columnar f Role
  , userCreatedAt     :: Columnar f UTCTime
  , userLastUpdatedAt :: Columnar f UTCTime
  }
  deriving (Generic, Beamable)

deriving instance Eq User
deriving instance Show User

type User = UserT Identity
type TodoId = PrimaryKey UserT Identity

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = UserId . userId


user :: IO User
user = do
  id  <- nextRandom
  now <- getCurrentTime
  return $ User id "John" "Doe" "john@doe.com" Customer now now
