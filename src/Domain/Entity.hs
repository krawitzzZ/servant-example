module Domain.Entity where

import           Data.UUID                      ( UUID )


class Entity a where
  identifier :: a -> UUID
