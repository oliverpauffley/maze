module Sidewinder where

import           App                  (MazeBuilder)
import           Control.Monad.Random as Random (fromList)
import           Control.Monad.RWS
import           Data.Map.Strict      as Map (lookup)
import           Data.Maybe           (catMaybes)
import           Maze                 (Edge (Edge, nodeID), Maze, MazeNode,
                                       Node (Node), NodeID, Path (Closed, Open),
                                       connect)

-- | Gets horizontal linked cells to the west
linkedCells :: Maze -> MazeNode -> [MazeNode]
linkedCells maze (Node _ _ _ _ _ w) = case w of
  Just (Edge nid Open) -> case Map.lookup nid maze of
    Just node -> node : linkedCells maze node
    Nothing   -> error "linked cells went wrong"
  Just (Edge _ Closed) -> []
  Nothing -> []

nodeToNodeWithNorthID :: MazeNode -> Maybe (NodeID, NodeID)
nodeToNodeWithNorthID (Node i _ n _ _ _) = case n of
  Just (Edge nid _) -> Just (i, nid)
  Nothing           -> Nothing

getChoices :: Maze -> MazeNode -> [((NodeID, NodeID), Rational)]
getChoices maze node@(Node i _ n _ e _) = eastProb ++ northProb
  where
    eastCell = (,) . nodeID <$> e <*> Just i
    eastProb = map (flip (,) 0.5) (catMaybes [eastCell])
    northCell = (,) . nodeID <$> n <*> Just i
    northCells = catMaybes $ northCell : map nodeToNodeWithNorthID (linkedCells maze node)
    northProb = map (flip (,) (0.5 / fromIntegral (length northCells))) northCells

generate :: (Monoid w) => NodeID -> MazeBuilder c w Maze ()
generate nid = do
  maze <- get
  let n = Map.lookup nid maze
  case n of
    (Just node) -> do
      let choices = getChoices maze node
      if null choices
        then return ()
        else do
          choice <- Random.fromList choices
          modify $ uncurry connect choice
    Nothing -> pure ()
