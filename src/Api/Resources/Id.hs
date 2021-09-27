module Api.Resources.Id where

import qualified Data.Aeson                    as JSON
import           Data.Aeson                     ( ToJSON(..) )
import           GHC.Generics                   ( Generic )
import           Utils                          ( jsonOptions )


newtype Dto i =  Dto { dtoId :: i } deriving (Eq, Show, Generic)

instance (ToJSON i) => ToJSON (Dto i) where
  toJSON = JSON.genericToJSON $ jsonOptions "dto"
