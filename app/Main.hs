{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import qualified BinaryTree
import qualified Control.Monad.RWS                as RWS
import           Control.Monad.Trans.State.Strict
import qualified Data.Map                         as Map
import           Draw                             (drawMaze)
import           Graphics.Gloss
import           Maze                             (Maze, NodeID (..), newMaze)
import           Options.Applicative
import           Param
import qualified Sidewinder
import           Solve                            (distance, solve)

main :: IO ()
main = do
  alg <- execParser (info (algorithm <**> helper) idm)
  run alg

run :: Algorithm -> IO ()
run alg = do
  case alg of
    BinaryTree cfg -> do
      maze <- runAlgorithm BinaryTree.generate cfg
      let solved = execState (distance 0 (NodeID (0, 0))) maze
      let solution = snd $ RWS.evalRWS solve () solved
      display (InWindow "Maze" (100, 100) (startWindowPos cfg.lineLength cfg.mazeSize)) white (drawMaze cfg.lineLength maze solution)
    Sidewinder cfg -> do
      maze <- runAlgorithm Sidewinder.generate cfg
      let solved = execState (distance 0 (NodeID (0, 0))) maze
      let solution = snd $ RWS.evalRWS solve () solved
      display (InWindow "Maze" (100, 100) (startWindowPos cfg.lineLength cfg.mazeSize)) white (drawMaze cfg.lineLength maze solution)
  where
    runAlgorithm generate c = do
      let m = newMaze c.mazeSize
      execStateT (traverse generate (Map.keys m)) m
