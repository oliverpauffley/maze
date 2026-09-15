{-# LANGUAGE FlexibleContexts #-}

-- | Implements the hunt and kill algorithm where we avoid walking on nodes we have already visited and search or "hunt" for unvisited nodes once we get trapped.
module Algorithm.HuntKill where

import Control.Monad.RWS (MonadState (get), modify')
import Control.Monad.Random (guard, uniform)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import MazeShape (
    GridShape,
    Maze,
    MazeBuilder,
    allCoords,
    connectEdge,
    getEdgesWith,
    randomNode,
 )

generateMaze :: (GridShape coord, Ord coord, Show coord) => MazeBuilder (Maze coord a) ()
generateMaze = get >>= randomNode >>= generate Set.empty

generate ::
    (GridShape coord, Ord coord, Show coord) =>
    Set.Set coord -> coord -> MazeBuilder (Maze coord a) ()
generate visited c = do
    maze <- get
    let visited' = Set.insert c visited
    if length visited' == length (allCoords maze)
        then pure ()
        else
            let choices = getEdgesWith c (unVisited visited') maze
             in if null choices
                    then hunt visited'
                    else do
                        next <- uniform choices
                        modify' $ connectEdge c next
                        generate visited' next

unVisited :: (Ord a) => Set.Set a -> a -> Bool
unVisited s c = Set.notMember c s

isVisited :: (Ord a) => Set.Set a -> a -> Bool
isVisited s c = Set.member c s

-- | Returns the coordinate of a node and a neighbour where the node hasn't been visited but the neighbour has.
withVisitedNeighbour :: (Ord b, GridShape b) => Maze b a -> Set.Set b -> b -> Maybe (b, b)
withVisitedNeighbour maze visited c = do
    guard $ Set.notMember c visited
    let conns = getEdgesWith c (isVisited visited) maze
    guard $ not (null conns)
    Just (c, head conns)

hunt ::
    (GridShape coord, Ord coord, Show coord) =>
    Set.Set coord -> MazeBuilder (Maze coord a) ()
hunt visited = do
    searchUnvisited visited >>= generate visited

-- | finds an unvisited node next to a visited one and connects them
searchUnvisited ::
    (GridShape coord, Ord coord, Show coord) =>
    Set.Set coord -> MazeBuilder (Maze coord a) coord
searchUnvisited visited = do
    m <- get
    let pairs = mapMaybe (withVisitedNeighbour m visited) (allCoords m)
    case pairs of
        [] -> error "could not find and unvisited Node"
        (conn@(next, _) : _) -> do
            modify' $ uncurry connectEdge conn
            pure next
