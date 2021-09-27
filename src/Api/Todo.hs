module Api.Todo where

import qualified Api.Resources.Id              as IdRes
import qualified Api.Resources.Todo            as TodoRes
import           Control.Monad.IO.Class         ( liftIO )
import           Data.UUID                      ( UUID )
import           Domain.Todo                    ( Todo(..)
                                                , createTodo
                                                )
import qualified Infrastructure.DbStash             as DB
import           Servant


type Api
  = "todos" :> Get '[JSON] [TodoRes.TodoResponseDto] :<|>
  ReqBody '[JSON] TodoRes.NewTodoRequestDto :> Post '[JSON] (IdRes.Dto UUID) :<|>
  Capture "id" UUID :>
  (
    Get '[JSON] TodoRes.TodoResponseDto :<|>
    ReqBody '[JSON] TodoRes.UpdateTodoRequestDto :> Put '[JSON] TodoRes.TodoResponseDto :<|>
    DeleteNoContent
  )

server :: DB.Persistance Todo -> Server Api
server db = getTodos :<|> newTodo :<|> operations
  where
  operations id = getTodo id :<|> updateTodo id :<|> deleteTodo id

  getTodos :: Handler [TodoRes.TodoResponseDto]
  getTodos = do
    todos <- liftIO $ DB.fetchAll db
    return $ map TodoRes.toResponseDto todos

  getTodo :: UUID -> Handler TodoRes.TodoResponseDto
  getTodo id = do
    maybeTodo <- liftIO $ DB.fetchOne db id
    case maybeTodo of
      Just todo -> return $ TodoRes.toResponseDto todo
      Nothing ->
        throwError $ err404 { errBody = "requested todo does not exist" }

  newTodo :: TodoRes.NewTodoRequestDto -> Handler (IdRes.Dto UUID)
  newTodo (TodoRes.NewTodoRequestDto title desc) = do
    todo     <- liftIO $ createTodo title desc
    eitherId <- liftIO $ DB.insert db todo
    case eitherId of
      Right id -> return $ IdRes.Dto id
      Left "duplicate record" ->
        throwError $ err409 { errBody = "duplicate record" }
      Left e -> throwError err500

  updateTodo :: UUID -> TodoRes.UpdateTodoRequestDto -> Handler TodoRes.TodoResponseDto
  updateTodo = undefined

  deleteTodo :: UUID -> Handler NoContent
  deleteTodo id = do
    _ <- liftIO $ DB.delete db id
    return NoContent
