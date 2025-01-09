{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import           App
import qualified BinaryTree
import           Data.Foldable       (traverse_)
import qualified Data.Map            as Map
import           Draw                (drawMaze)
import           Graphics.Gloss
import           Maze                (NodeID (..), newMaze)
import           Options.Applicative
import           Param
import qualified Sidewinder
import           Solve               (distance, solve)

main :: IO ()
main = do
  customExecParser p opts >>= run
  where
    opts =
      info
        (algorithm <**> helper)
        (fullDesc <> progDesc "Create and solve random mazes!")
    p = prefs showHelpOnEmpty

run :: Algorithm -> IO ()
run alg = do
  case alg of
    BinaryTree cfg -> do
      runAlgorithm BinaryTree.generate cfg
    Sidewinder cfg -> do
      runAlgorithm Sidewinder.generate cfg
  pure ()
  where
    runAlgorithm generate c = do
      let m = newMaze c.mazeSize
      (picture, maze, solution) <- runBuilder (traverse_ generate (Map.keys m) >> distance 0 (NodeID (0, 0)) >> Solve.solve >> drawMaze) c m
      display (InWindow "Maze" (100, 100) (startWindowPos c.lineLength c.mazeSize)) white picture
