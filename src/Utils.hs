module Utils
  ( jsonOptions
  , getUuid
  ) where

import           Control.Arrow                  ( (>>>) )
import qualified Data.Aeson                    as JSON
import           Data.Char                      ( toLower
                                                , toUpper
                                                )
import           Data.UUID                      ( UUID )
import           Data.UUID.V4                   ( nextRandom )


getUuid :: IO UUID
getUuid = nextRandom

jsonOptions :: String -> JSON.Options
jsonOptions prefix = JSON.defaultOptions
  { JSON.fieldLabelModifier = dropPrefix >>> firstToLower
  }
  where dropPrefix = drop $ length prefix

firstToLower :: String -> String
firstToLower []         = []
firstToLower (c : rest) = toLower c : rest
