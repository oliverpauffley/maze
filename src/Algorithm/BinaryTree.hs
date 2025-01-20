-- | Implements the binary tree algorithm for maze generation.
-- Walks the maze an either cuts a path south or east.
module Algorithm.BinaryTree (generateMaze) where

import           App                  (MazeBuilder, getNode)
import           Control.Monad.Random
import           Control.Monad.RWS
import           Data.Foldable        (traverse_)
import qualified Data.Map             as Map
import           Data.Maybe           (catMaybes)
import           Maze                 (Edge (Edge), Maze, Node (Node), NodeID,
                                       connect)

generate :: (Monoid w) => NodeID -> MazeBuilder c w Maze ()
generate nid = do
  (Node a _ n _ e _)<- getNode nid
  let choices = catMaybes [n, e]
  if null choices
    then return ()
    else do
    (Edge b _) <- uniform $ catMaybes [n, e]
    modify' $ connect a b

generateMaze :: (Monoid w) => MazeBuilder c w Maze ()
generateMaze = do
  keys <- gets Map.keys
  traverse_ generate keys
