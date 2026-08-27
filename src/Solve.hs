{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Solve where

import Control.Lens (view, (&), (?~), (^.))
import Control.Monad (guard)
import Control.Monad.RWS (MonadState (get, put), gets, modify')
import Control.Monad.Random (MonadRandom, uniform)
import Data.Foldable (Foldable (foldMap'), maximumBy, minimumBy, traverse_)
import Data.Function (on)
import Data.Functor.Rep (Representable (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Traversable (for)
import MazeShape

-- | a node position with it's distance from a starting postion.
type NodeDistance coord = (coord, Int)

{- | walk through the maze and assign the distance walked as the value of the node we reach.
for each step increase the distance until we have walked all nodes in the maze
-}
distance :: (GridShape coord, Ord coord) => Int -> Maze coord () -> coord -> Map coord Int
distance d maze coord = go d maze coord mempty
  where
    go dis m c values =
        case Map.lookup c values of
            -- if we have already set a value here we should just return
            Just _ -> values
            Nothing ->
                let nextNodes = getOpenEdges c m
                 in Map.insert c dis values <> foldMap' (\n -> go (dis + 1) m n values) nextNodes

solveNode :: (Show coord, GridShape coord, Ord coord) => coord -> Maze coord Int -> [coord]
solveNode coord maze =
    let val = getNodeValue coord maze
        connections = getOpenEdges coord maze
     in case val of
            Nothing -> error $ "trying to solve and we have walked to a node not on the grid" <> show coord
            Just v ->
                case findLowestPath v connections maze of
                    Nothing -> []
                    Just next -> next : solveNode next maze

findLowestPath :: (Ord coord) => Int -> [coord] -> Maze coord Int -> Maybe coord
findLowestPath val connections m = do
    let values = filter ((< val) . snd) $ mapMaybe (\c -> (c,) <$> getNodeValue c m) connections
    case values of
        [] -> Nothing
        xs -> (Just . fst . minimum) xs

solve :: (MonadRandom m, Show coord, GridShape coord, Ord coord) => Maze coord () -> m [coord]
solve maze = do
    start <- randomNode maze
    let walkedMaze = distance 0 maze start
        mx = maxElement walkedMaze
        maze' = Maze walkedMaze (maze ^. mazeEdges)
    return $ solveNode mx maze'

-- solve :: (Representable d, Bounded (Rep d), Enum (Rep d)) => MazeBuilder (Maze d) [NodeID]
-- solve = do
--     m <- get
--     solveNode $ fst $ Map.findMax m

-- -- find the longestPath in the maze
-- solveLongest :: (Representable d, Bounded (Rep d), Enum (Rep d)) => MazeBuilder (Maze d) [NodeID]
-- solveLongest = do
--     m <- get
--     solveNode $ maxElement m

maxElement :: Map coord Int -> coord
maxElement m = fst $ maximumBy (compare `on` snd) (Map.assocs m)

-- -- | Finds a longest route through a maze by applying Djkstra's algorithm twice
-- findLongestRoute :: (Representable d, Bounded (Rep d), Enum (Rep d)) => MazeBuilder (Maze d) [NodeID]
-- findLongestRoute = do
--     blankMaze <- get -- get the un-solved maze

--     -- solve once
--     start <- randomNode
--     distance 0 (start ^. nid)

--     -- plot the distances from the highestElement

--     newStart <- gets maxElement

--     -- reset and solve again
--     put blankMaze
--     distance 0 newStart >> solveLongest
