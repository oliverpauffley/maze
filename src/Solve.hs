module Solve where

import           App               (MazeBuilder, getNode, randomNode)
import           Control.Monad.RWS
import           Data.Foldable     (for_, maximumBy, minimumBy, traverse_)
import           Data.Function     (on)
import qualified Data.Map          as Map
import           Data.Maybe        (isJust, mapMaybe)
import           Data.Traversable  (for)
import           Maze              (Maze, Node (Node, nid, value),
                                    NodeID (NodeID), openConnections)

-- | a node position with it's distance from a starting postion.
type NodeDistance = (NodeID, Int)

distance ::  Int -> NodeID -> MazeBuilder c Maze ()
distance d nid = do
  node@(Node _ val _ _ _ _) <- getNode nid
  if isJust val
    then return ()
    else do
      modify' $ setValue d nid
      let toUpdate = openConnections node
      traverse_ (distance (d + 1)) toUpdate

-- update the value in the node within the map
setValue :: Int -> NodeID -> Maze -> Maze
setValue d = Map.adjust (\x -> x {value = Just d})

solveNode :: NodeID -> MazeBuilder c Maze [NodeID]
solveNode nid = do
  m <- get
  node@(Node _ val _ _ _ _) <- getNode nid
  case val of
    (Just v) -> do
      let paths = openConnections node
          next = findLowestPath v paths m
      nids <- for next solveNode
      return $ nid : concat nids
    Nothing -> pure []

findLowestPath :: Int -> [NodeID] -> Maze -> Maybe NodeID
findLowestPath val ns m = do
  let vs = filter ((< val) . snd) $ mapMaybe (flip nodeValue m) ns
  if null vs
    then Nothing
    else Just $ fst (minimumBy (compare `on` snd) vs)
  where
    nodeValue i m = Map.lookup i m >>= (\v -> (,) i <$> v) . value

solve :: MazeBuilder c Maze [NodeID]
solve = do
  m <- get
  solveNode $ fst $ Map.findMax m

-- find the longestPath in the maze
solveLongest :: MazeBuilder c Maze [NodeID]
solveLongest = do
  m <- get
  solveNode $ maxElement m

maxElement :: (Ord b) => Map.Map a (Node b e) -> a
maxElement m = fst $ maximumBy (compare `on` value . snd) (Map.assocs m)

-- | Finds a longest route through a maze by applying Djkstra's algorithm twice
findLongestRoute :: MazeBuilder c Maze [NodeID]
findLongestRoute = do
  blankMaze <- get -- get the un-solved maze

  -- solve once
  start <- randomNode
  distance 0 (nid start)

  -- plot the distances from the highestElement

  newStart <- gets maxElement

  -- reset and solve again
  put blankMaze
  distance 0 newStart >> solveLongest
