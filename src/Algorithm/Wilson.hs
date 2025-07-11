{-# LANGUAGE FlexibleContexts #-}

module Algorithm.Wilson (generateMaze) where

import Control.Monad.RWS (
    MonadIO (liftIO),
    MonadState (get),
    modify',
 )
import Control.Monad.Random (uniform)
import Data.Foldable (traverse_)
import Data.Functor.Rep (Representable (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import MazeShape (
    Maze,
    MazeBuilder,
    Node,
    NodeID,
    Opposite,
    closedConnections,
    connectNodes,
    connections,
    connectionsWith,
    directions,
    eID,
    getNode,
    nid,
    toList,
 )

-- | a path of connections that we want to make.
type Path d = [(NodeID, Rep d)]

generateMaze ::
    (Representable d, Opposite (Rep d), Eq (Rep d), Bounded (Rep d), Enum (Rep d), Show (Rep d)) => MazeBuilder (Maze d) ()
generateMaze = do
    m <- get
    let unvisited = Set.fromList $ Map.keys m
    unvisited' <- liftIO $ deleteRandom unvisited
    newStart unvisited'

generate ::
    (Representable d, Opposite (Rep d), Eq (Rep d), Bounded (Rep d), Enum (Rep d), Show (Rep d)) =>
    Set.Set NodeID ->
    Path d ->
    NodeID ->
    MazeBuilder (Maze d) ()
generate unvisited path nid = do
    m <- get
    let n = getNode m nid
    (nextID, dir) <- uniform (closedConnections n)
    let next = (nid, dir)
    let path' = updatePath path next
    if Set.member nextID unvisited
        then
            generate unvisited path' nextID
        else do
            connectPath unvisited path'

connectPath ::
    (Representable d, Opposite (Rep d), Eq (Rep d), Bounded (Rep d), Enum (Rep d), Show (Rep d)) =>
    Set.Set NodeID ->
    Path d ->
    MazeBuilder (Maze d) ()
connectPath unvisited path = do
    connectAll path
    let unvisited' = deleteAll unvisited (map fst path)
    if null unvisited'
        then pure ()
        else newStart unvisited'

newStart ::
    (Representable d, Opposite (Rep d), Eq (Rep d), Bounded (Rep d), Enum (Rep d), Show (Rep d)) =>
    Set.Set NodeID ->
    MazeBuilder (Maze d) ()
newStart unvisited = do
    m <- get
    nextID <- uniform unvisited
    let node = getNode m nextID
    (_, dir) <- uniform $ closedConnections node
    generate unvisited [(nextID, dir)] nextID

deleteRandom :: (Ord a) => Set.Set a -> IO (Set.Set a)
deleteRandom ss = do
    vis <- uniform ss
    return $ Set.delete vis ss

connectAll :: (Representable d, Opposite (Rep d), Eq (Rep d), Show (Rep d)) => Path d -> MazeBuilder (Maze d) ()
connectAll = traverse_ (\(a, b) -> modify' (connectNodes a b))

deleteAll :: (Ord a) => Set.Set a -> [a] -> Set.Set a
deleteAll = foldr Set.delete

-- | check the current path for a loop, if it exists remove it

-- >>> updatePath [(0,0), (1,0), (1,1), (0,1)] (0,0)
-- [(0,0)]

-- >>> updatePath [(0,0), (1,0), (1,1), (2,1), (2,2), (1,2)] (1,1)
-- [(0,0),(1,1)]
updatePath :: (Eq a) => [(a, b)] -> (a, b) -> [(a, b)]
updatePath xs n@(a, _) = ys ++ [n]
  where
    (ys, _) = break ((== a) . fst) xs
