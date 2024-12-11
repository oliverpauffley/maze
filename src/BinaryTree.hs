-- | Implements the binary tree algorithm for maze generation.
-- Walks the maze an either cuts a path south or east.
module BinaryTree where

import           Control.Monad.Random
import           Control.Monad.State
import           Data.Foldable        (traverse_)
import qualified Data.Map             as Map
import           Data.Maybe           (catMaybes)
import           Debug.Trace          (traceShow)
import           Maze                 (Edge (Edge), Maze, MazeNode, Node (Node),
                                       connect)

generate :: MazeNode -> StateT Maze IO ()
generate (Node a _ n _ e _) = do
  let choices = catMaybes [n, e]
  if null choices
    then return ()
    else do
      (Edge b _) <- uniform $ catMaybes [n, e]
      connect a b
      return ()

generateMaze :: StateT Maze IO ()
generateMaze = do
  m <- get
  traverse_ generate (Map.elems m)
