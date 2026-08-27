{-# LANGUAGE FlexibleContexts #-}

{- | Implements the binary tree algorithm for maze generation.
Walks the maze an either cuts a path south or east.
-}
module Algorithm.BinaryTree (generateMaze) where

import Control.Monad.RWS
import Control.Monad.Random
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import MazeShape (
    GridShape (..),
    Maze,
    MazeBuilder,
    NorthEastDirection,
    allCoords,
    connectEdge,
    getNorthEastNeighbors,
 )

generate ::
    (GridShape coord, NorthEastDirection (Direction coord), Ord coord) => coord -> MazeBuilder (Maze coord a) ()
generate c = do
    let (n, e) = getNorthEastNeighbors c
    let choices = catMaybes $ [n, e]
    if null choices
        then return ()
        else do
            new <- uniform choices
            modify' $ connectEdge c new

generateMaze :: (GridShape coord, NorthEastDirection (Direction coord), Ord coord) => MazeBuilder (Maze coord a) ()
generateMaze = do
    keys <- gets allCoords
    traverse_ generate keys
