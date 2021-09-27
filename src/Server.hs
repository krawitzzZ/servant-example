module Server where

import qualified Api.Todo                      as Todo
import qualified Api.User                      as User
import           Database.Beam                  ( insert
                                                , insertValues
                                                , runInsert
                                                )
import           Database.Beam.Sqlite           ( runBeamSqliteDebug )
import           Database.SQLite.Simple         ( open )
import           DummyData                      ( defaultBeamTodos
                                                , defaultTodos
                                                )
import           Infrastructure.Db              ( AppDatabase(..)
                                                , appDatabase
                                                )
import           Infrastructure.DbStash         ( initDbWithDefaultData )
import           Infrastructure.Entities.Todo   ( toEntity )
import           Network.Wai                    ( Application )
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Servant.Errors
                                                ( errorMwDefJson )
import           Servant


type Api = "api" :> ( Todo.Api :<|> User.Api )

getServer :: IO (Server Api)
getServer = do
  initTodos     <- defaultTodos
  initBeamTodos <- defaultBeamTodos
  db            <- initDbWithDefaultData initTodos
  -- conn          <- open "todosdb1.db"
  -- runBeamSqliteDebug putStrLn conn
  --   $ runInsert
  --   $ insert (todos appDatabase)
  --   $ insertValues initBeamTodos -- TODO figure out how to make UUID DB compatible
  return $ Todo.server db :<|> User.server

api :: Proxy Api
api = Proxy

app :: IO Application
app = do
  errorMwDefJson . serve api <$> getServer

run :: Int -> IO ()
run port = do
  let settings = setPort port $ setBeforeMainLoop
        (putStrLn $ "TodoServer is listening on port " ++ show port)
        defaultSettings
  runSettings settings =<< app
