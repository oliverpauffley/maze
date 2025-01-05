module Main where

import           Options.Applicative
import           Param

main :: IO ()
main = do
  alg <- execParser (info (algorithm <**> helper) idm)
  run alg
