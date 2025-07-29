{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Algorithm.Sidewinder (generateMaze) where

import Control.Lens ((^.))
import Control.Monad.RWS
import Control.Monad.Random as Random (fromList)
import Data.Foldable (traverse_)
import Data.Functor.Rep (Representable (..))
import Data.Map.Strict as Map (keys, lookup)
import Data.Maybe (catMaybes)
import GridKind (CardinalDir (..), FromCardinalDir (..))
import MazeShape

-- | Gets horizontal linked cells to the west
linkedCells :: (FromCardinalDir (Rep d), Representable d) => Maze d -> MazeNode d -> [MazeNode d]
linkedCells maze n =
    case index (n ^. directions) (fromCardinalDir North) of
        Just (Edge nid Open) -> case Map.lookup nid maze of
            Just node -> node : linkedCells maze node
            Nothing -> error "linked cells went wrong"
        Just (Edge _ Closed) -> []
        Nothing -> []

nodeToNodeWithRep :: (FromCardinalDir (Rep d), Representable d) => MazeNode d -> Maybe (NodeID, Rep d)
nodeToNodeWithRep (Node i _ dirs) = (i, fromCardinalDir North) <$ index dirs (fromCardinalDir North)

getChoices :: (FromCardinalDir (Rep d), Representable d) => Maze d -> MazeNode d -> [((NodeID, Rep d), Rational)]
getChoices maze node@(Node i _ dirs) = eastProb ++ northProb
  where
    (e, n) = (index dirs (fromCardinalDir East), index dirs (fromCardinalDir North))
    eastCell = (,) . const i <$> e <*> Just (fromCardinalDir East)
    eastProb = map (,0.5) (catMaybes [eastCell])
    northCell = (,) . const i <$> n <*> Just (fromCardinalDir North)
    northCells = catMaybes $ northCell : map nodeToNodeWithRep (linkedCells maze node)
    northProb = map (,0.5 / fromIntegral (length northCells)) northCells

generate ::
    (FromCardinalDir (Rep d), Representable d, Eq (Rep d), Opposite (Rep d)) => NodeID -> MazeBuilder (Maze d) ()
generate nid = do
    maze <- get
    let
        node = getNode maze nid
        choices = getChoices maze node
    if null choices
        then return ()
        else do
            choice <- Random.fromList choices
            modify $ uncurry connectNodes choice

generateMaze :: (FromCardinalDir (Rep d), Representable d, Eq (Rep d), Opposite (Rep d)) => MazeBuilder (Maze d) ()
generateMaze = do
    ks <- gets Map.keys
    traverse_ generate ks
