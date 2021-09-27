module Api.User where

import           Servant


type Api = "users" :> Get '[PlainText] String

server :: Server Api
server = return "hey from users API"
