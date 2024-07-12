module Main where

import           Grid

main :: IO ()
main = do
  putStrLn $ prettyPrintGrid exampleGrid
