{-# LANGUAGE FlexibleContexts #-}

{- | Implements the binary tree algorithm for maze generation.
Walks the maze an either cuts a path south or east.
-}
module Algorithm.BinaryTree (generateMaze) where

import Control.Monad.RWS (MonadState (get), gets, modify')
import Control.Monad.Random (uniform)
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
    (GridShape coord, NorthEastDirection (Direction coord), Ord coord, Show coord) => coord -> MazeBuilder (Maze coord a) ()
generate c = do
    m <- get
    let (n, e) = getNorthEastNeighbors m c
    let choices = catMaybes $ [n, e]
    if null choices
        then return ()
        else do
            next <- uniform choices
            modify' $ connectEdge c next

generateMaze ::
    (GridShape coord, NorthEastDirection (Direction coord), Ord coord, Show coord) => MazeBuilder (Maze coord a) ()
generateMaze = do
    keys <- gets allCoords
    traverse_ generate keys
