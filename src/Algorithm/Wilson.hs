{-# LANGUAGE FlexibleContexts #-}

module Algorithm.Wilson (generateMaze) where

import Control.Monad.RWS (
    MonadIO (liftIO),
    MonadState (get),
    modify',
 )
import Control.Monad.Random (uniform)
import Data.Foldable (traverse_)
import qualified Data.Set as Set
import MazeShape (
    GridShape (Direction),
    Maze,
    MazeBuilder,
    allCoords,
    connectEdge,
    getClosedEdges,
    mazeNodes,
 )

-- | a path of connections that we want to make.
type Path coord = [(coord, coord)]

-- TODO something going wrong here I think!
generateMaze :: (GridShape coord, Ord coord, Show coord) => MazeBuilder (Maze coord a) ()
generateMaze = do
    m <- get
    let unvisited = Set.fromList $ allCoords m
    unvisited' <- liftIO $ deleteRandom unvisited
    newStart unvisited'

generate ::
    (GridShape coord, Ord coord, Show coord) =>
    Set.Set coord ->
    Path coord ->
    coord ->
    MazeBuilder (Maze coord a) ()
generate unvisited path c = do
    m <- get
    nextCoord <- uniform (getClosedEdges c m)
    let path' = updatePath path (c, nextCoord)
    if Set.member nextCoord unvisited
        then
            generate unvisited path' nextCoord
        else do
            connectPath unvisited path'

connectPath ::
    (GridShape coord, Ord coord, Show coord) =>
    Set.Set coord ->
    Path coord ->
    MazeBuilder (Maze coord a) ()
connectPath unvisited path = do
    connectAll path
    let unvisited' = unvisited `Set.difference` (pathToSet path)
    if null unvisited'
        then pure ()
        else newStart unvisited'

newStart ::
    (GridShape coord, Ord coord, Show coord) =>
    Set.Set coord ->
    MazeBuilder (Maze coord a) ()
newStart unvisited = do
    m <- get
    nextCoord <- uniform unvisited
    nextCoord' <- uniform $ getClosedEdges nextCoord m
    let nextPath = [(nextCoord, nextCoord')]
    generate unvisited nextPath nextCoord

deleteRandom :: (Ord a) => Set.Set a -> IO (Set.Set a)
deleteRandom ss = do
    vis <- uniform ss
    return $ Set.delete vis ss

connectAll ::
    (GridShape coord, Ord coord, Show coord) => Path coord -> MazeBuilder (Maze coord a) ()
connectAll = traverse_ (\(a, b) -> modify' (connectEdge a b))

-- | check the current path for a loop, if it exists remove it

-- >>> updatePath [((0,0), (1,0)), ((1,0), (1,1)), ((1,1), (0,1)), ((0,1), (0,0))] ((0,0), (1,0))
-- [((0,0),(1,0))]

-- >>> updatePath [(0,0), (1,0), (1,1), (2,1), (2,2), (1,2)] (1,1)
-- [(0,0),(1,1)]
updatePath :: (Eq a) => [(a, a)] -> (a, a) -> [(a, a)]
updatePath xs n@(next, _) = ys ++ [n]
  where
    (ys, _) = break ((== next) . fst) xs

pathToSet :: (Ord a) => [(a, a)] -> Set.Set a
pathToSet xs = foldr (\(a, b) s -> insertBoth a b s) mempty xs
  where
    insertBoth a b s = Set.insert b (Set.insert a s)
