{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import qualified Algorithm.AldousBroder       as AldousBroder
import qualified Algorithm.BinaryTree         as BinaryTree
import qualified Algorithm.HuntKill           as HuntKill
import qualified Algorithm.RecursiveBacktrack as RecursiveBacktrack
import qualified Algorithm.Sidewinder         as Sidewinder
import qualified Algorithm.Wilson             as Wilson
import           App
import           DeadEnds                     (getDeadEnds)
import           Draw                         (drawMaze)
import           Graphics.Gloss
import           Maze                         (newMaze)
import           Options.Applicative
import           Param
import           Solve                        (findLongestRoute)

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
    Wilson cfg -> do
      runAlgorithm Wilson.generateMaze cfg
    HuntKill cfg -> do
      runAlgorithm HuntKill.generateMaze cfg
    RecursiveBacktrack cfg -> do
      runAlgorithm RecursiveBacktrack.generateMaze cfg
  pure ()
  where
    runAlgorithm generate c = do
      let m = newMaze c.mazeSize
      (deadEnds, maze, solution) <- runBuilder (generate >> Solve.findLongestRoute >> getDeadEnds) c m
      (picture, _, _) <- runBuilder (drawMaze solution deadEnds) c maze
      display (InWindow "Maze" (100, 100) (startWindowPos c.lineLength c.mazeSize)) white picture
