module Main where

import qualified TodoServer

main :: IO ()
main = do
  TodoServer.run 3000
