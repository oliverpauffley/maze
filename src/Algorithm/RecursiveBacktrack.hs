{-# LANGUAGE FlexibleContexts #-}

-- | Randomly walks on unvisited nodes. When it reaches a node surrounded by visited cells it backtracks and tries again
module Algorithm.RecursiveBacktrack (generateMaze) where

import Control.Monad.RWS (MonadState (get), modify')
import Control.Monad.Random (uniform)
import qualified Data.Set as Set
import MazeShape (
    GridShape,
    Maze,
    MazeBuilder,
    connectEdge,
    getEdgesWith,
    randomNode,
 )

generateMaze :: (GridShape coord, Ord coord, Show coord) => MazeBuilder (Maze coord a) ()
generateMaze = do
    m <- get
    start <- randomNode m
    generate Set.empty [start]

generate ::
    (GridShape coord, Ord coord, Show coord) =>
    Set.Set coord -> [coord] -> MazeBuilder (Maze coord a) ()
generate _ [] = pure ()
generate s ns@(x : xs) = do
    m <- get
    let
        s' = Set.insert x s
        choices = getEdgesWith x (\e -> Set.notMember e s) m
    if null choices
        then generate s' xs
        else do
            next <- uniform choices
            modify' $ connectEdge x next
            generate s' (next : ns)
