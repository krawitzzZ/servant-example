module Domain.Role where

import           GHC.Generics                   ( Generic )

data Role = Customer | Admin deriving (Eq, Show, Generic)
