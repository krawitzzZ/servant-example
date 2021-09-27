module Main where

import qualified Server

main :: IO ()
main = do
  Server.run 3000
