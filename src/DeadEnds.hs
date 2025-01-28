-- | reports on the number of dead ends in a maze
module DeadEnds where

import           App               (MazeBuilder)
import           Control.Monad.RWS
import qualified Data.Map          as Map
import           Maze              (Maze, MazeNode, Node (nid), NodeID,
                                    openConnections)

getDeadEnds :: MazeBuilder c Maze [NodeID]
getDeadEnds = do
  m <- get
  let nodes = Map.elems m
  return $ nid <$> filter isDeadEnd nodes

-- | a node is dead end if it has only a single open connection
isDeadEnd :: MazeNode -> Bool
isDeadEnd node =
  length (openConnections node) == 1
