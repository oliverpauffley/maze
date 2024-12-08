-- | Implements the binary tree algorithm for maze generation.
-- Walks the maze an either cuts a path south or east.
module BinaryTree where

import           Control.Monad.Random
import qualified Data.Map             as Map
import           Data.Maybe           (catMaybes)
import           Maze                 (Edge (Edge), Maze, MazeNode, Node (Node),
                                       connect)

generate :: (MonadRandom m) => m Maze -> MazeNode -> m Maze
generate maze (Node a _ n _ e _) = do
  let choices = catMaybes [n, e]
  if null choices
    then maze
    else do
      (Edge b _) <- uniform $ catMaybes [n, e]
      m <- maze
      return $ connect m a b

generateMaze :: (MonadRandom m) => Maze -> m Maze
generateMaze maze = go (pure maze) (Map.elems maze)
  where
    go m [] = m
    go m (n : ns) = do
      newMaze <- generate m n
      go (pure newMaze) ns
