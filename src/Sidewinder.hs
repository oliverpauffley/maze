module Sidewinder where

import           Control.Monad.Random             as Random
import           Control.Monad.Trans.State.Strict
import           Data.Foldable                    (traverse_)
import           Data.Map.Strict                  as Map hiding (map, null)
import           Data.Maybe                       (catMaybes)
import           Maze                             (Edge (Edge, nodeID), Maze,
                                                   MazeNode, Node (Node),
                                                   NodeID, Path (Open), connect)

-- | Gets horizontal linked cells to the west
linkedCells :: Maze -> MazeNode -> [MazeNode]
linkedCells maze (Node _ _ _ _ _ w) = case w of
  Just (Edge nodeID Open) -> case Map.lookup nodeID maze of
    Just node -> node : linkedCells maze node
    Nothing   -> error "linked cells went wrong"
  _ -> []

nodeToNodeWithNorthID :: MazeNode -> Maybe (NodeID, NodeID)
nodeToNodeWithNorthID (Node id _ n _ _ _) = case n of
  Just (Edge nodeID _) -> Just (id, nodeID)
  Nothing              -> Nothing

generate :: MazeNode -> StateT Maze IO ()
generate node = do
  maze <- get
  let choices = getChoices maze node
  if null choices
    then return ()
    else do
      choice <- Random.fromList choices
      uncurry connect choice

getChoices :: Maze -> MazeNode -> [((NodeID, NodeID), Rational)]
getChoices maze node@(Node id _ n _ e _) = eastProb ++ northProb
  where
    eastCell = (,) . nodeID <$> e <*> Just id
    eastProb = map (flip (,) 0.5) (catMaybes [eastCell])
    northCell = (,) . nodeID <$> n <*> Just id
    northCells = catMaybes $ northCell : map nodeToNodeWithNorthID (linkedCells maze node)
    northProb = map (flip (,) (0.5 / fromIntegral (length northCells))) northCells

generateMaze :: StateT Maze IO ()
generateMaze = do
  m <- get
  mapM_ generate (Map.elems m)
