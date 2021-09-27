module Infrastructure.Entities.Todo where

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
import qualified Domain.Todo                   as D
import           GHC.Generics                   ( Generic )


data TodoT f = Todo
  { todoId            :: Columnar f UUID
  , todoTitle         :: Columnar f Text
  , todoDescription   :: Columnar f Text
  , todoIsComplete    :: Columnar f Bool
  , todoCreatedAt     :: Columnar f UTCTime
  , todoLastUpdatedAt :: Columnar f UTCTime
  }
  deriving (Generic, Beamable)

deriving instance Eq Todo
deriving instance Show Todo

type Todo = TodoT Identity
type TodoId = PrimaryKey TodoT Identity

instance Table TodoT where
  data PrimaryKey TodoT f = TodoId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = TodoId . todoId

toDomain :: Todo -> D.Todo
toDomain (Todo uuid title desc complete created updated) =
  D.Todo uuid title desc complete created updated

toEntity :: D.Todo -> Todo
toEntity (D.Todo uuid title desc complete created updated) =
  Todo uuid title desc complete created updated

todo :: IO Todo
todo = do
  id  <- nextRandom
  now <- getCurrentTime
  return $ Todo id "title" "description" False now now
