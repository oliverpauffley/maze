{-# LANGUAGE TupleSections #-}

-- | Mask nodes in the maze so that they cannot be used in the generation of mazes
module Mask where

import           App               (MazeBuilder, getNode)
import           Control.Monad.RWS
import           Data.Foldable     (traverse_)
import qualified Data.Map          as Map
import           Maze              (Edge (nodeID), Maze, NodeID, paths, remove)

killNode ::  NodeID -> MazeBuilder c Maze ()
killNode i = do
  node <- getNode i
  -- clear this node from all neigbours
  let neighbours = nodeID <$> paths node
  traverse_ ((\(a, b) -> modify' $ remove a b) . (,i)) neighbours
  -- clear from map
  modify' $ Map.delete i

killNodes :: (Traversable t) => t NodeID -> MazeBuilder c Maze ()
killNodes = traverse_ killNode
