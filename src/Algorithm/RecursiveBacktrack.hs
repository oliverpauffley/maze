-- | Randomly walks on unvisited nodes. When it reaches a node surrounded by visited cells it backtracks and tries again
module Algorithm.RecursiveBacktrack (generateMaze) where

import           App                  (MazeBuilder, getNode, randomNode)
import           Control.Monad.Random (uniform)
import           Control.Monad.RWS
import qualified Data.Set             as Set
import           Maze                 (Edge (Edge), Maze, Node (nid), NodeID,
                                       connect, connectionsWith)

generateMaze :: (Monoid w) => MazeBuilder c w Maze ()
generateMaze = do
  start <- nid <$> randomNode
  generate Set.empty [start]

generate :: (Monoid w) => Set.Set NodeID -> [NodeID] -> MazeBuilder c w Maze ()
generate _ [] = pure ()
generate s ns@(x : xs) = do
  node <- getNode x
  let choices = connectionsWith (\(Edge i _) -> Set.notMember i s) node
      s' = Set.insert x s
  if null choices
    then generate s' xs
    else do
      next <- uniform choices
      modify' $ connect x next
      generate s' (next : ns)
