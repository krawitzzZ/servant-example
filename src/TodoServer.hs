module TodoServer where

import qualified Api.Todo                      as Todo
import           Network.Wai                    ( Application )
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Servant.Errors
                                                ( errorMwDefJson )
import           Servant


type Api = "api" :> Todo.Api

server :: Server Api
server = Todo.server

api :: Proxy Api
api = Proxy

app :: IO Application
app = return $ errorMwDefJson $ serve api server

run :: Int -> IO ()
run port = do
  let settings = setPort port $ setBeforeMainLoop
        (putStrLn $ "TodoServer is listening on port " ++ show port)
        defaultSettings
  runSettings settings =<< app
