module Algorithm.Wilson (generateMaze) where

import           App                  (MazeBuilder, getNode)
import           Control.Monad.Random
import           Control.Monad.RWS
import           Data.Foldable        (traverse_)
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Maze                 (Maze, NodeID, connect, connections)

generateMaze :: MazeBuilder c Maze ()
generateMaze = do
  m <- get
  let unvisited = Set.fromList $ Map.keys m
  unvisited' <- liftIO $ deleteRandom unvisited
  start <- uniform unvisited'
  generate unvisited' [start] start

generate :: Set.Set NodeID -> [NodeID] -> NodeID -> MazeBuilder c Maze ()
generate unvisited path nid = do
  n <- getNode nid
  nextID <- uniform (connections n)
  let path' = updatePath path nextID
  if Set.member nextID unvisited
    then
      generate unvisited path' nextID
    else do
      connectPath unvisited path'

connectPath :: Set.Set NodeID -> [NodeID] -> MazeBuilder c Maze ()
connectPath unvisited path = do
  connectAll path
  let unvisited' = deleteAll unvisited path
  if null unvisited'
    then pure ()
    else newStart unvisited'

newStart :: Set.Set NodeID -> MazeBuilder c Maze ()
newStart unvisited = do
  ns <- uniform unvisited
  generate unvisited [ns] ns

deleteRandom :: (Ord a) => Set.Set a -> IO (Set.Set a)
deleteRandom ss = do
  vis <- uniform ss
  return $ Set.delete vis ss

connectAll :: [NodeID] -> MazeBuilder c Maze ()
connectAll ns = traverse_ (\(a, b) -> modify' (connect a b)) toConnect
  where
    toConnect = zip ns (drop 1 ns)

deleteAll :: (Ord a) => Set.Set a -> [a] -> Set.Set a
deleteAll = foldr Set.delete

-- | check the current path for a loop, if it exists remove it

-- >>> updatePath [(0,0), (1,0), (1,1), (0,1)] (0,0)
-- [(0,0)]

-- >>> updatePath [(0,0), (1,0), (1,1), (2,1), (2,2), (1,2)] (1,1)
-- [(0,0),(1,0),(1,1)]
updatePath :: (Eq a) => [a] -> a -> [a]
updatePath xs n = ys ++ [n]
  where
    (ys, _) = break (== n) xs
