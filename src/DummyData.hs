module DummyData where

import           Data.Time                      ( getCurrentTime )
import           Domain.Todo                    ( Todo(..) )
import qualified Infrastructure.Entities.Todo  as T
import           Utils                          ( getUuid )


defaultTodos :: IO [Todo]
defaultTodos = mapM todoWithId defaultTodoData
 where
  todoWithId (title, description, done) = do
    id  <- getUuid
    now <- getCurrentTime
    return $ Todo id title description done now now

  defaultTodoData =
    [ ("Chores"   , "Clean up this mess"                    , False)
    , ("Haskell"  , "Learn haskell"                         , False)
    , ("Servant"  , "Learn about servant framework"         , False)
    , ("Todos app", "Write todo app using servant framework", False)
    ]

defaultBeamTodos :: IO [T.Todo]
defaultBeamTodos = map T.toEntity <$> defaultTodos
