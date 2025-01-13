-- | Implements the binary tree algorithm for maze generation.
-- Walks the maze an either cuts a path south or east.
module BinaryTree where

import           App                  (MazeBuilder)
import           Control.Monad.Random
import           Control.Monad.RWS
import           Data.Foldable        (traverse_)
import qualified Data.Map             as Map
import           Data.Maybe           (catMaybes)
import           Maze                 (Edge (Edge), Maze, Node (Node), NodeID,
                                       connect)

generate :: (Monoid w) => NodeID -> MazeBuilder c w Maze ()
generate nid = do
  node <- gets (Map.lookup nid)
  case node of
    (Just (Node a _ n _ e _)) -> do
      let choices = catMaybes [n, e]
      if null choices
        then return ()
        else do
          (Edge b _) <- uniform $ catMaybes [n, e]
          modify' $ connect a b
    Nothing -> pure ()

generateMaze :: (Monoid w) => MazeBuilder c w Maze ()
generateMaze = do
  keys <- gets Map.keys
  traverse_ generate keys
