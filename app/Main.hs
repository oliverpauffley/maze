{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedRecordDot       #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import qualified Algorithm.AldousBroder       as AldousBroder
import qualified Algorithm.BinaryTree         as BinaryTree
import qualified Algorithm.HuntKill           as HuntKill
import qualified Algorithm.RecursiveBacktrack as RecursiveBacktrack
import qualified Algorithm.Sidewinder         as Sidewinder
import qualified Algorithm.Wilson             as Wilson
import           App
import           DeadEnds                     (getDeadEnds)
import           Diagrams.Backend.SVG         (renderSVG)
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude             hiding (connect)
import           Draw                         (drawMaze)
import           Image                        (maskToBlankMaze)
import           Maze                         (Edge (..), Edges, Maze,
                                               NodeID (..), Path (..), connect,
                                               newMaze)
import           Options.Applicative
import           Param
import           Solve                        (findLongestRoute)

-- main :: IO ()
-- main = do
--   (diagram, _) <- m
--   mainWith $ diagram
--   where
--     c = Config 1 30 False False False False Nothing
--     m = runBuilder (BinaryTree.generateMaze >> Solve.findLongestRoute >> drawMaze [] []) c (newMaze c.mazeSize)

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
    runAlgorithm :: MazeBuilder Config Maze () -> Config -> IO ()
    runAlgorithm generate c = do
      let m = newMaze c.mazeSize
      (solution, maze) <- runBuilder (maskToBlankMaze >> generate >> Solve.findLongestRoute) c m
      (deadEnds, maze') <- runBuilder getDeadEnds c maze
      (picture, _) <- runBuilder (drawMaze solution deadEnds) c maze'
      renderSVG "maze.svg" (mkWidth c.diagramSize) picture
