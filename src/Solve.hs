{-# LANGUAGE FlexibleContexts #-}

module Solve where

import Control.Lens (view, (&), (?~), (^.))
import Control.Monad.RWS (MonadState (get, put), gets, modify')
import Data.Foldable (maximumBy, minimumBy, traverse_)
import Data.Function (on)
import Data.Functor.Rep (Representable (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Traversable (for)
import MazeShape (
    Edge,
    Maze,
    MazeBuilder,
    MazeNode,
    Node (..),
    NodeID (NodeID),
    Path (Open),
    connectionsWith,
    eID,
    getNode,
    nid,
    openConnections,
    path,
    randomNode,
    value,
 )

-- | a node position with it's distance from a starting postion.
type NodeDistance = (NodeID, Int)

distance :: (Representable d, Bounded (Rep d), Enum (Rep d)) => Int -> NodeID -> MazeBuilder (Maze d) ()
distance d nid = do
    m <- get
    let node@(Node _ val _) = getNode m nid
    if isJust val
        then return ()
        else do
            modify' $ setValue d nid
            let toUpdate = openConnections node
            traverse_ (distance (d + 1)) toUpdate

-- update the value in the node within the map
setValue :: Int -> NodeID -> Maze d -> Maze d
setValue d = Map.adjust (\x -> x & value ?~ d)

solveNode :: (Representable d, Bounded (Rep d), Enum (Rep d)) => NodeID -> MazeBuilder (Maze d) [NodeID]
solveNode nid = do
    m <- get
    let node@(Node _ val _) = getNode m nid
    case val of
        (Just v) -> do
            let paths = openConnections node
                next = findLowestPath v paths m
            nids <- for next solveNode
            return $ nid : concat nids
        Nothing -> pure []

findLowestPath :: Int -> [NodeID] -> Maze d -> Maybe NodeID
findLowestPath val ns m = do
    let vs = filter ((< val) . snd) $ mapMaybe (nodeValue m) ns
    if null vs
        then Nothing
        else Just $ fst (minimumBy (compare `on` snd) vs)
  where
    nodeValue m i = Map.lookup i m >>= (\v -> (,) i <$> v) . view value

solve :: (Representable d, Bounded (Rep d), Enum (Rep d)) => MazeBuilder (Maze d) [NodeID]
solve = do
    m <- get
    solveNode $ fst $ Map.findMax m

-- find the longestPath in the maze
solveLongest :: (Representable d, Bounded (Rep d), Enum (Rep d)) => MazeBuilder (Maze d) [NodeID]
solveLongest = do
    m <- get
    solveNode $ maxElement m

maxElement :: Map NodeID (Node d (Maybe Int) Path) -> NodeID
maxElement m = fst $ maximumBy (compare `on` view value . snd) (Map.assocs m)

-- | Finds a longest route through a maze by applying Djkstra's algorithm twice
findLongestRoute :: (Representable d, Bounded (Rep d), Enum (Rep d)) => MazeBuilder (Maze d) [NodeID]
findLongestRoute = do
    blankMaze <- get -- get the un-solved maze

    -- solve once
    start <- randomNode
    distance 0 (start ^. nid)

    -- plot the distances from the highestElement

    newStart <- gets maxElement

    -- reset and solve again
    put blankMaze
    distance 0 newStart >> solveLongest
