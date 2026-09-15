{-# LANGUAGE FlexibleContexts #-}

-- | reports on the number of dead ends in a maze
module DeadEnds where

import Control.Lens ((^.))
import qualified Data.Map as Map
import MazeShape (
    GridShape,
    Maze,
    getOpenEdges,
    mazeNodes,
 )

getDeadEnds :: (Ord coord, GridShape coord) => Maze coord a -> Map.Map coord a
getDeadEnds m = do
    let nodes = m ^. mazeNodes
    Map.filterWithKey (isDeadEnd m) nodes

-- | a node is dead end if it has only a single open connection
isDeadEnd :: (Ord coord, GridShape coord) => Maze coord a -> coord -> a -> Bool
isDeadEnd maze coord _ =
    length (getOpenEdges coord maze) == 1
