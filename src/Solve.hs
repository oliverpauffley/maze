module Solve where

import           App               (MazeBuilder)
import           Control.Monad.RWS
import           Data.Foldable     (for_, maximumBy, minimumBy, traverse_)
import           Data.Function     (on)
import qualified Data.Map          as Map
import           Data.Maybe        (isJust, mapMaybe)
import           Maze              (Maze, Node (Node, value), NodeID (NodeID),
                                    openPaths)

-- | a node position with it's distance from a starting postion.
type NodeDistance = (NodeID, Int)

distance :: (Monoid w) => Int -> NodeID -> MazeBuilder c w Maze ()
distance d nid = do
  m <- get
  case Map.lookup nid m of
    Just node@(Node _ val _ _ _ _) ->
      if isJust val
        then return ()
        else do
          modify' $ setValue d nid
          let toUpdate = openPaths node
          traverse_ (distance (d + 1)) toUpdate
    Nothing -> return ()

-- update the value in the node within the map
setValue :: Int -> NodeID -> Maze -> Maze
setValue d = Map.adjust (\x -> x {value = Just d})

solveNode :: NodeID -> MazeBuilder c [NodeID] Maze ()
solveNode nid = do
  m <- get
  tell [nid]
  case Map.lookup nid m of
    Just node@(Node _ val _ _ _ _) ->
      case val of
        (Just v) -> do
          let paths = openPaths node
              next = findLowestPath v paths m
          for_ next solveNode
        Nothing -> pure ()
    Nothing -> pure ()

findLowestPath :: Int -> [NodeID] -> Maze -> Maybe NodeID
findLowestPath val ns m = do
  let vs = filter ((< val) . snd) $ mapMaybe (flip nodeValue m) ns
  if null vs
    then Nothing
    else Just $ fst (minimumBy (compare `on` snd) vs)
  where
    nodeValue i m = Map.lookup i m >>= (\v -> (,) i <$> v) . value

solve :: MazeBuilder c [NodeID] Maze ()
solve = do
  m <- get
  solveNode $ fst $ Map.findMax m

-- find the longestPath in the maze
solveLongest :: MazeBuilder c [NodeID] Maze ()
solveLongest = do
  m <- get
  solveNode $ maxElement m

maxElement :: (Ord b) => Map.Map a (Node b e) -> a
maxElement m = fst $ maximumBy (compare `on` value . snd) (Map.assocs m)

-- | Finds a longest route through a maze by applying Djkstra's algorithm twice
findLongestRoute :: MazeBuilder c [NodeID] Maze ()
findLongestRoute = do
  blankMaze <- get -- get the un-solved maze

  -- solve once
  distance 0 (NodeID (0, 0))

  -- plot the distances from the highestElement

  newStart <- gets maxElement

  -- reset and solve again
  put blankMaze
  distance 0 newStart >> solveLongest
