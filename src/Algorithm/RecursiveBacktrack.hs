{-# LANGUAGE FlexibleContexts #-}

-- | Randomly walks on unvisited nodes. When it reaches a node surrounded by visited cells it backtracks and tries again
module Algorithm.RecursiveBacktrack (generateMaze) where

import Control.Lens (view)
import Control.Lens.Getter ((^.))
import Control.Monad.RWS
import Control.Monad.Random (uniform)
import Data.Functor.Rep (Representable (..))
import qualified Data.Set as Set
import MazeShape (
    Edge (Edge),
    Maze,
    MazeBuilder,
    NodeID,
    Opposite,
    connectNodes,
    connectionsWith,
    eID,
    getNode,
    nid,
    randomNode,
 )

generateMaze ::
    (Representable d, Opposite (Rep d), Eq (Rep d), Bounded (Rep d), Enum (Rep d)) => MazeBuilder (Maze d) ()
generateMaze = do
    start <- view nid <$> randomNode
    generate Set.empty [start]

generate ::
    (Representable d, Opposite (Rep d), Eq (Rep d), Bounded (Rep d), Enum (Rep d)) =>
    Set.Set NodeID ->
    [NodeID] ->
    MazeBuilder (Maze d) ()
generate _ [] = pure ()
generate s ns@(x : xs) = do
    m <- get
    let node = getNode m x
        s' = Set.insert x s
        choices = connectionsWith (\e -> Set.notMember (e ^. eID) s) node
    if null choices
        then generate s' xs
        else do
            (next, dir) <- uniform choices
            modify' $ connectNodes x dir
            generate s' ((next ^. eID) : ns)
