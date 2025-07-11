{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import qualified Algorithm.AldousBroder as AldousBroder
import qualified Algorithm.BinaryTree as BinaryTree
import qualified Algorithm.HuntKill as HuntKill
import qualified Algorithm.RecursiveBacktrack as RecursiveBacktrack
import qualified Algorithm.Sidewinder as Sidewinder
import qualified Algorithm.Wilson as Wilson
import DeadEnds (getDeadEnds)
import Diagrams.Backend.SVG (renderSVG)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (connect)
import Draw (drawMaze)
import MazeShape (
    Config (..),
    Edge (..),
    Maze,
    MazeBuilder,
    NodeID (..),
    Path (..),
    runBuilder,
 )
import MazeShape.Square (Cardinal (Cardinal), newSquareGrid)
import Options.Applicative
import Param
import Solve (findLongestRoute)

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
    runAlgorithm :: MazeBuilder (Maze Cardinal) () -> Config -> IO ()
    runAlgorithm generate c = do
        let m = newSquareGrid c.mazeSize
        (solution, maze) <- runBuilder (generate >> Solve.findLongestRoute) c m
        (deadEnds, maze') <- runBuilder getDeadEnds c maze
        (picture, _) <- runBuilder (drawMaze solution deadEnds) c maze'
        renderSVG "maze.svg" (mkWidth c.diagramSize) picture
