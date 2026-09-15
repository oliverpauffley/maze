{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Solve where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Random (MonadRandom)
import Data.Foldable (Foldable (foldMap'), maximumBy)
import Data.Function (on)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import MazeShape (
    GridShape,
    Maze (Maze),
    getNodeValue,
    getOpenEdges,
    mazeEdges,
    mazeNodes,
    randomNode,
 )

-- | a node position with it's distance from a starting postion.
type NodeDistance coord = (coord, Int)

{- | walk through the maze and assign the distance walked as the value of the node we reach.
for each step increase the distance until we have walked all nodes in the maze
-}
distance :: (GridShape coord, Ord coord) => Int -> Maze coord () -> coord -> Maze coord Int
distance d maze coord =
    let newNodes = (go d maze coord mempty)
     in maze & mazeNodes .~ newNodes
  where
    go dis m c values =
        case Map.lookup c values of
            -- if we have already set a value here we should just return
            Just _ -> values
            Nothing ->
                let nextNodes = getOpenEdges c m
                 in Map.insert c dis values <> foldMap' (\n -> go (dis + 1) m n values) nextNodes

solveNodes :: (Show coord, GridShape coord, Ord coord) => coord -> Maze coord Int -> [coord]
solveNodes coord maze =
    let val = getNodeValue coord maze
        connections = getOpenEdges coord maze
     in case val of
            Nothing -> error $ "trying to solve and we have walked to a node not on the grid" <> show coord
            Just v ->
                case findLowestPath v connections maze of
                    Nothing -> []
                    Just next -> next : solveNodes next maze

findLowestPath :: (Ord coord) => Int -> [coord] -> Maze coord Int -> Maybe coord
findLowestPath val connections m = do
    let values = filter ((< val) . snd) $ mapMaybe (\c -> (c,) <$> getNodeValue c m) connections
    case values of
        [] -> Nothing
        xs -> (Just . fst . minimum) xs

solve :: (Show coord, GridShape coord, Ord coord) => Maze coord Int -> [coord]
solve maze =
    let
        nodes = maze ^. mazeNodes
        mx = maxElement nodes
        maze' = Maze nodes (maze ^. mazeEdges)
     in
        solveNodes mx maze'

maxElement :: Map coord Int -> coord
maxElement m = fst $ maximumBy (compare `on` snd) (Map.assocs m)

-- | Finds a longest route through a maze by applying Djkstra's algorithm twice
findLongestRoute :: (MonadRandom m, Show b, GridShape b, Ord b) => Maze b () -> m [b]
findLongestRoute blankMaze = do
    -- start anywhere
    start <- randomNode blankMaze
    -- walk the maze to figure out the highest element in maze
    let walkedMaze = distance 0 blankMaze (start)
        -- find the point furthest from the random start
        newStart = maxElement $ walkedMaze ^. mazeNodes
        -- rewalk from this point furthest from the start
        walkedMaze' = distance 0 blankMaze (newStart)
    -- solve returns the point furthest from this point
    return $ solve walkedMaze'
