module Infrastructure.DbStash where

import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.Cache                    as C
import           Data.Text                      ( Text )
import           Data.Time                      ( getCurrentTime )
import           Data.UUID                      ( UUID )
import           Domain.Entity                  ( Entity(identifier) )
import           Domain.Todo                    ( Todo(..) )
import           Utils                          ( getUuid )


type Persistance a = C.Cache UUID a

initDb :: Entity a => IO (Persistance a)
initDb = C.newCache Nothing

initDbWithDefaultData :: Entity a => [a] -> IO (Persistance a)
initDbWithDefaultData defaultData = do
  db <- C.newCache Nothing
  mapM_ (\entity -> C.insert db (identifier entity) entity) defaultData
  return db

fetchAll :: Entity a => Persistance a -> IO [a]
fetchAll db = do
  list <- C.toList db
  return $ map (\(_, entity, _) -> entity) list

fetchOne :: Entity a => Persistance a -> UUID -> IO (Maybe a)
fetchOne = C.lookup'

insert :: Entity a => Persistance a -> a -> IO (Either Text UUID)
insert db entity = do
  maybeEntity <- fetchOne db (identifier entity)
  case maybeEntity of
    Just _  -> return $ Left "duplicate record"
    Nothing -> do
      C.insert db (identifier entity) entity
      return . Right $ identifier entity

updateTodo :: Entity a => Persistance a -> a -> IO (Either String a)
updateTodo db entity = do
  maybeEntity <- liftIO $ C.lookup' db (identifier entity)
  case maybeEntity of
    Nothing       -> return $ Left "not found"
    Just dbEntity -> do
      liftIO $ C.insert db (identifier entity) entity
      return $ Right entity

delete :: Entity a => Persistance a -> UUID -> IO ()
delete = C.delete
