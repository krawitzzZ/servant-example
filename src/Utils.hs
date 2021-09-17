module Utils
  ( toJsonOptions
  , parseJsonOptions
  ) where

import           Control.Arrow                  ( (<<<)
                                                , (>>>)
                                                )
import qualified Data.Aeson                    as JSON
import           Data.Char                      ( toLower
                                                , toUpper
                                                )

toJsonOptions :: String -> JSON.Options
toJsonOptions prefix = JSON.defaultOptions
  { JSON.fieldLabelModifier = dropPrefix >>> firstToLower
  }
  where dropPrefix = drop $ length prefix

parseJsonOptions :: String -> JSON.Options
parseJsonOptions prefix = JSON.defaultOptions
  { JSON.fieldLabelModifier = addPrefix <<< firstToUpper
  }
  where addPrefix = (prefix ++)

firstToLower :: String -> String
firstToLower []         = []
firstToLower (c : rest) = toLower c : rest

firstToUpper :: String -> String
firstToUpper []         = []
firstToUpper (c : rest) = toUpper c : rest
