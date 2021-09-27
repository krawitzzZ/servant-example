module Domain.Permission where

import           GHC.Generics                   ( Generic )


data Permission = CreateUser | DeleteUser deriving (Eq, Show, Generic)
