{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Algorithm.Sidewinder (generateMaze) where

import Control.Monad.RWS (MonadState (get), gets, modify')
import Control.Monad.Random as Random (fromList)
import Data.Foldable (traverse_)
import Data.Maybe (maybeToList)
import MazeShape (
    EdgeState (Closed, Open),
    GridShape (Direction),
    Maze,
    MazeBuilder,
    NorthEastDirection,
    allCoords,
    connectEdge,
    getEdgeState,
    getNorth,
    getNorthEastNeighbors,
 )

-- | Gets horizontal linked cells to the west
linkedCells ::
    (GridShape coord, NorthEastDirection (Direction coord), Ord coord) => Maze coord a -> coord -> [(coord, coord)]
linkedCells maze c =
    case getNorth c of
        Nothing -> []
        Just (c') -> case getEdgeState c c' maze of
            Nothing -> []
            Just Open -> (c, c') : linkedCells maze c'
            Just Closed -> []

getChoices ::
    (GridShape coord, NorthEastDirection (Direction coord), Ord coord) =>
    Maze coord a -> coord -> [((coord, coord), Rational)]
getChoices maze c = eastProb ++ northProb
  where
    (n, e) = getNorthEastNeighbors maze c
    eastProb = maybeToList $ (,0.5) . (c,) <$> e
    northCells = case n of
        Nothing -> linkedCells maze c
        Just n' -> (c, n') : linkedCells maze c
    northProb = map (,0.5 / fromIntegral (length northCells)) northCells

generate ::
    (GridShape coord, NorthEastDirection (Direction coord), Ord coord, Show coord) => coord -> MazeBuilder (Maze coord a) ()
generate c = do
    maze <- get
    let
        choices = getChoices maze c
    if null choices
        then return ()
        else do
            choice <- Random.fromList choices
            modify' $ uncurry connectEdge choice

generateMaze ::
    (GridShape coord, NorthEastDirection (Direction coord), Ord coord, Show coord) => MazeBuilder (Maze coord a) ()
generateMaze = do
    ks <- gets allCoords
    traverse_ generate ks
