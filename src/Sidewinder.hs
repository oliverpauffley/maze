module Sidewinder where

import           Control.Monad.Random             as Random
import           Control.Monad.Trans.State.Strict
import           Data.Foldable                    (traverse_)
import           Data.Map.Strict                  as Map hiding (map, null)
import           Data.Maybe                       (catMaybes)
import           Debug.Trace                      (traceShow)
import           Maze                             (Edge (Edge, nodeID), Maze,
                                                   MazeNode, Node (Node),
                                                   NodeID, Path (Closed, Open),
                                                   connect)

-- | Gets horizontal linked cells to the west
linkedCells :: Maze -> MazeNode -> [MazeNode]
linkedCells maze (Node _ _ _ _ _ w) = case w of
  Just (Edge nodeID Open) -> case Map.lookup nodeID maze of
    Just node -> node : linkedCells maze node
    Nothing   -> error "linked cells went wrong"
  Just (Edge _ Closed) -> []
  Nothing -> []

nodeToNodeWithNorthID :: MazeNode -> Maybe (NodeID, NodeID)
nodeToNodeWithNorthID (Node id _ n _ _ _) = case n of
  Just (Edge nodeID _) -> Just (id, nodeID)
  Nothing              -> Nothing

generate :: NodeID -> StateT Maze IO ()
generate nid = do
  maze <- get
  let n = Map.lookup nid maze
  case n of
    (Just node) -> do
      let choices = getChoices maze node
      if null choices
        then return ()
        else do
          choice <- traceShow choices Random.fromList choices
          modify $ uncurry connect choice
    Nothing -> pure ()

getChoices :: Maze -> MazeNode -> [((NodeID, NodeID), Rational)]
getChoices maze node@(Node id _ n _ e _) = eastProb ++ northProb
  where
    eastCell = (,) . nodeID <$> e <*> Just id
    eastProb = map (flip (,) 0.5) (catMaybes [eastCell])
    northCell = (,) . nodeID <$> n <*> Just id
    northCells = traceShow ((linkedCells maze node), node) catMaybes $ northCell : map nodeToNodeWithNorthID (linkedCells maze node)
    northProb = map (flip (,) (0.5 / fromIntegral (length northCells))) northCells

-- generateMaze :: StateT Maze IO ()
-- generateMaze = do
--   m <- get
--   mapM_ generate (Map.elems m)
