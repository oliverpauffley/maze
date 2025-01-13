{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import qualified AldousBroder
import           App
import qualified BinaryTree
import           Draw                (drawMaze)
import           Graphics.Gloss
import           Maze                (newMaze)
import           Options.Applicative
import           Param
import qualified Sidewinder
import           Solve               (findLongestRoute)

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
      runAlgorithm BinaryTree.generateMaze cfg
    Sidewinder cfg -> do
      runAlgorithm Sidewinder.generateMaze cfg
    AldousBroder cfg -> do
      runAlgorithm AldousBroder.generateMaze cfg
  pure ()
  where
    runAlgorithm generate c = do
      let m = newMaze c.mazeSize
      (_, maze, solution) <- runBuilder (generate >> Solve.findLongestRoute) c m
      (picture, _, _) <- runBuilder (drawMaze solution) c maze
      display (InWindow "Maze" (100, 100) (startWindowPos c.lineLength c.mazeSize)) white picture
