module Server where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson                    as JSON
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString                ( ByteString )
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.Time                      ( UTCTime(UTCTime)
                                                , fromGregorian
                                                , secondsToDiffTime
                                                )
import           Data.Time.Calendar
import           GHC.Generics
import           Lucid
import           Network.HTTP.Media             ( (//)
                                                , (/:)
                                                )
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Prelude                        ( )
import           Prelude.Compat
import           Servant
import           Servant.Types.SourceT          ( source )
import           System.Directory
import           System.IO
import           Text.Blaze
import qualified Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Utf8
import           Utils                          ( parseJsonOptions
                                                , toJsonOptions
                                                )


data User = User
  { userId               :: Integer
  , userFirstName        :: String
  , userLastName         :: String
  , userAge              :: Integer
  , userEmail            :: String
  , userRegistrationDate :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON User where
  toJSON = JSON.genericToJSON $ toJsonOptions "user"


-- type UserApi =
--   "users" :> QueryParam "sortBy" SortBy :> Get '[JSON] (Headers '[Header "User-Count" Integer] [User]) :<|>
--   "users" :> Capture "userId" Integer :> Get '[JSON] User :<|>
--   "users" :> Header "Tenant" Tenant :> ReqBody '[JSON] UserDto :> PostCreated '[JSON] User :<|>
--   "users" :> Header "Tenant" Tenant :> Capture "userId" Integer :> ReqBody '[JSON] UserDto :> Put '[JSON] User :<|>
--   "users" :> Header "Tenant" Tenant :> Capture "userId" Integer :> DeleteNoContent

type GetUsersAPI
  = "users" :> Get '[JSON] [User] :<|> "users" :> Capture "userId" Integer :> Get '[JSON] User

getUsers :: Handler [User]
getUsers = return users

getUser :: Integer -> Handler User
getUser id = case maybeUser of
  Nothing   -> throwError err404
  Just user -> return user
  where maybeUser = find (\user -> userId user == id) users

server :: Server GetUsersAPI
server = getUsers :<|> getUser

getUsersAPI :: Proxy GetUsersAPI
getUsersAPI = Proxy

mkApp :: IO Application
mkApp = return $ serve getUsersAPI server

run :: Int -> IO ()
run port = do
  let settings = setPort port $ setBeforeMainLoop
        (putStrLn $ "Server is listening on port " ++ show port)
        defaultSettings
  runSettings settings =<< mkApp





users :: [User]
users =
  [ User 1
         "Isaac"
         "Newton"
         372
         "isaac@newton.co.uk"
         (UTCTime (fromGregorian 1683 3 1) (secondsToDiffTime 0))
  , User 2
         "Albert"
         "Einstein"
         136
         "ae@mc2.org"
         (UTCTime (fromGregorian 1905 12 1) (secondsToDiffTime 0))
  , User 3
         "Bob"
         "The Great"
         136
         "btg@bobbo.com"
         (UTCTime (fromGregorian 1905 12 1) (secondsToDiffTime 0))
  ]
