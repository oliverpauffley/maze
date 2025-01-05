-- | Implements the binary tree algorithm for maze generation.
-- Walks the maze an either cuts a path south or east.
module BinaryTree where

import           Control.Monad.Random
import           Control.Monad.Trans.State.Strict
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes)
import           Maze                             (Edge (Edge), Maze,
                                                   Node (Node), NodeID, connect)

generate :: NodeID -> StateT Maze IO ()
generate nid = do
  node <- gets (Map.lookup nid)
  case node of
    (Just (Node a _ n _ e _)) -> do
      let choices = catMaybes [n, e]
      if null choices
        then return ()
        else do
          (Edge b _) <- uniform $ catMaybes [n, e]
          modify $ connect a b
    Nothing -> pure ()
