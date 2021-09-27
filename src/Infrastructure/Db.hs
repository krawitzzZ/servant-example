module Infrastructure.Db where

import           Data.UUID                      ( UUID )
import           Database.Beam                  ( Database
                                                , DatabaseSettings
                                                , TableEntity
                                                , defaultDbSettings
                                                )
import           Database.Beam.Backend          ( HasSqlValueSyntax )
import           GHC.Generics                   ( Generic )
import           Infrastructure.Entities.Todo   ( TodoT )
import           Infrastructure.Entities.User   ( UserT )


-- instance HasSqlValueSyntax UUID where -- TODO how to use UUUD for this lib???


data AppDatabase f = AppDatabase
  { todos :: f (TableEntity TodoT)
  , users :: f (TableEntity UserT)
  }
  deriving (Generic, Database be)

appDatabase :: DatabaseSettings be AppDatabase
appDatabase = defaultDbSettings
