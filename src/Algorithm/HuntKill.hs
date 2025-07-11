{-# LANGUAGE FlexibleContexts #-}

-- | Implements the hunt and kill algorithm where we avoid walking on nodes we have already visited and search or "hunt" for unvisited nodes once we get trapped.
module Algorithm.HuntKill where

import Control.Lens ((^.))
import Control.Monad.RWS (MonadState (get), modify')
import Control.Monad.Random (guard, uniform)
import Control.Monad.Representable.Reader (Representable)
import Data.Functor.Rep (Representable (..))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import MazeShape (
    Edge (..),
    Maze,
    MazeBuilder,
    MazeNode,
    Node (..),
    NodeID (NodeID),
    Opposite,
    connectNodes,
    connections,
    connectionsWith,
    directions,
    eID,
    getNode,
    nid,
    randomNode,
 )

generateMaze ::
    (Representable d, Bounded (Rep d), Enum (Rep d), Eq (Rep d), Opposite (Rep d)) => MazeBuilder (Maze d) ()
generateMaze = randomNode >>= generate Set.empty . _nid

generate ::
    (Representable d, Bounded (Rep d), Enum (Rep d), Eq (Rep d), Opposite (Rep d)) =>
    Set.Set NodeID ->
    NodeID ->
    MazeBuilder (Maze d) ()
generate visited i = do
    maze <- get
    let visited' = Set.insert i visited
    if length visited' == length maze
        then pure ()
        else
            let choices = connectionsWith (unVisited visited') (getNode maze i)
             in if null choices
                    then hunt visited'
                    else do
                        next <- uniform choices
                        modify' $ connectNodes i (snd next)
                        generate visited' (fst next ^. eID)

unVisited :: Set.Set NodeID -> (Edge e -> Bool)
unVisited s (Edge i _) = Set.notMember i s

isVisited :: Set.Set NodeID -> (Edge e -> Bool)
isVisited s (Edge i _) = Set.member i s

-- | Returns the node ids of a node and direction to it's neighbour where the node hasn't been visited but the neighbour has.
withVisitedNeighbour ::
    (Representable d, Bounded (Rep d), Enum (Rep d), Eq (Rep d), Opposite (Rep d)) =>
    Set.Set NodeID ->
    MazeNode d ->
    Maybe (NodeID, Rep d)
withVisitedNeighbour ss node = do
    let conns = connections node
        id = node ^. nid
    guard $ Set.notMember id ss
    let xs = map snd $ filter (\(edge, _) -> Set.member (edge ^. eID) ss) conns
    guard $ not (null xs)
    Just (id, head xs)

hunt ::
    (Representable d, Bounded (Rep d), Enum (Rep d), Eq (Rep d), Opposite (Rep d)) =>
    Set.Set NodeID ->
    MazeBuilder (Maze d) ()
hunt visited = do
    searchUnvisited visited >>= generate visited

-- | finds an unvisited node next to a visited one and connects them
searchUnvisited ::
    (Representable d, Bounded (Rep d), Enum (Rep d), Eq (Rep d), Opposite (Rep d)) =>
    Set.Set NodeID ->
    MazeBuilder (Maze d) NodeID
searchUnvisited visited = do
    m <- get
    let pairs = mapMaybe (withVisitedNeighbour visited) (Map.elems m)
    case pairs of
        [] -> error "could not find and unvisited Node"
        (conn@(next, _) : _) -> do
            modify' $ uncurry connectNodes conn
            pure next
