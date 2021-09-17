module Api.Todo where

import           Data.List                      ( find )
import           Domain.Todo                    ( Todo(..)
                                                , dummyTodos
                                                )
import           Servant


type Api
  = "todos" :> Get '[JSON] [Todo] :<|> "todos" :> Capture "todoId" Integer :> Get '[JSON] Todo

server :: Server Api
server = getTodos :<|> getTodo

 where
  getTodos :: Handler [Todo]
  getTodos = return dummyTodos

  getTodo :: Integer -> Handler Todo
  getTodo id = case maybeTodo of
    Just todo -> return todo
    Nothing ->
      throwError $ err404 { errBody = "requested todo does not exist" }
    where maybeTodo = find (\todo -> todoId todo == id) dummyTodos
