-- | Implements the hunt and kill algorithm where we avoid walking on nodes we have already visited and search or "hunt" for unvisited nodes once we get trapped.
module Algorithm.HuntKill where

import           App                  (MazeBuilder, randomNode)
import           Control.Monad.Random (guard, uniform)
import           Control.Monad.RWS
import qualified Data.Map             as Map
import           Data.Maybe           (catMaybes, mapMaybe)
import qualified Data.Set             as Set
import           Maze                 (Edge (..), Maze, MazeNode, Node (..),
                                       NodeID (NodeID), connect,
                                       connectionsWith)

generateMaze :: MazeBuilder c Maze ()
generateMaze = randomNode >>= generate Set.empty . nid

generate :: Set.Set NodeID -> NodeID -> MazeBuilder c Maze ()
generate visited i = do
  maze <- get
  let visited' = Set.insert i visited
  if length visited' == length maze
    then pure ()
    else case Map.lookup i maze of
      Nothing -> pure ()
      (Just n) -> do
        let choices = connectionsWith (unVisited visited') n
        if null choices
          then hunt visited'
          else do
            next <- uniform choices
            modify' $ connect next i
            generate visited' next

unVisited :: Set.Set NodeID -> (Edge e -> Bool)
unVisited s (Edge i _) = Set.notMember i s

isVisited :: Set.Set NodeID -> (Edge e -> Bool)
isVisited s (Edge i _) = Set.member i s

-- | finds an unvisited node next to a visited one and connects them
searchUnvisited :: Set.Set NodeID -> MazeBuilder c Maze NodeID
searchUnvisited visited = do
  m <- get
  let pairs = mapMaybe (withVisitedNeighbour visited) (Map.elems m)
  case pairs of
    [] -> error "could not find and unvisited Node"
    ((next, connection) : _) -> do
      modify' $ connect next connection
      pure next

-- | Returns the node ids of a node and it's neighbour where the node hasn't been visited but the neighbour has.
withVisitedNeighbour :: Set.Set NodeID -> MazeNode -> Maybe (NodeID, NodeID)
withVisitedNeighbour ss (Node i _ n s e w) = do
  guard $ Set.notMember i ss
  let xs = filter (`Set.member` ss) (map nodeID $ catMaybes [n, s, e, w])
  guard $ not (null xs)
  Just (i, head xs)

hunt :: Set.Set NodeID -> MazeBuilder c Maze ()
hunt visited = do
  searchUnvisited visited >>= generate visited
