module Sidewinder where

import           Control.Monad.Random      as Random
import           Control.Monad.Trans.State
import           Data.Foldable             (traverse_)
import           Data.Map                  as Map hiding (map, null)
import           Data.Maybe                (catMaybes)
import           Maze

-- | Gets horizontal linked cells to the west
linkedCells :: Maze -> MazeNode -> [MazeNode]
linkedCells maze (Node _ _ _ _ _ w) = case w of
  Just (Edge nodeID Open) -> case Map.lookup nodeID maze of
    Just node -> node : linkedCells maze node
    Nothing   -> error "linked cells went wrong"
  _ -> []

nodeToNorthID :: MazeNode -> Maybe NodeID
nodeToNorthID (Node _ _ n _ _ _) = case n of
  Just (Edge nodeID _) -> Just nodeID
  Nothing              -> Nothing

generate :: MazeNode -> StateT Maze IO ()
generate node@(Node a _ _ _ _ _) = do
  maze <- get
  let choices = getChoices maze node
  if null choices
    then return ()
    else do
      choice <- Random.fromList choices
      connect a choice

getChoices :: Maze -> MazeNode -> [(NodeID, Rational)]
getChoices maze node@(Node _ _ n _ e _) = eastProb ++ northProb
  where
    eastCell = nodeID <$> e
    eastProb = map (flip (,) 0.5) (catMaybes [eastCell])
    northCell = nodeID <$> n
    northCells = catMaybes $ northCell : map nodeToNorthID (linkedCells maze node)
    northProb = map (flip (,) (0.5 / fromIntegral (length northCells))) northCells

generateMaze :: StateT Maze IO ()
generateMaze = do
  m <- get
  traverse_ generate (Map.elems m)
