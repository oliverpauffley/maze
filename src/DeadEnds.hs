-- | reports on the number of dead ends in a maze
module DeadEnds where
import           App  (MazeBuilder)
import           Maze (Maze, NodeID)

getDeadEnds :: (Monoid w) => MazeBuilder c w Maze [NodeID]
getDeadEnds = undefined
