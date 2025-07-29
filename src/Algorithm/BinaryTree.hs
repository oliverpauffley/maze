{-# LANGUAGE FlexibleContexts #-}

{- | Implements the binary tree algorithm for maze generation.
Walks the maze an either cuts a path south or east.
-}
module Algorithm.BinaryTree (generateMaze) where

import Control.Monad.RWS
import Control.Monad.Random
import Data.Foldable (traverse_)
import Data.Functor.Rep (Representable (..))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import GridKind (FromCardinalDir)
import MazeShape (
    Edge (Edge),
    Maze,
    MazeBuilder,
    Node (Node),
    NodeID,
    Opposite,
    connectNodes,
    getNode,
 )
import MazeShape.Square (northEastDirections)

generate ::
    (FromCardinalDir (Rep d), Representable d, Eq (Rep d), Opposite (Rep d)) => NodeID -> MazeBuilder (Maze d) ()
generate nid = do
    m <- get
    let choices = catMaybes . northEastDirections $ getNode m nid
    if null choices
        then return ()
        else do
            (dir, _) <- uniform choices
            modify' $ connectNodes nid dir

generateMaze :: (FromCardinalDir (Rep d), Representable d, Eq (Rep d), Opposite (Rep d)) => MazeBuilder (Maze d) ()
generateMaze = do
    keys <- gets Map.keys
    traverse_ generate keys
