module Maze where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)

-- all mazes are square so width is height too
type Width = Int

newtype NodeID = NodeID (Int, Int)
  deriving (Show, Eq)

instance Ord NodeID where
  compare (NodeID (x1, y1)) (NodeID (x2, y2))
    | x1 == x2 && y1 == y2 = EQ
    | y1 > y2 = GT
    | y1 < y2 = LT
    | x1 < x2 = LT
    | otherwise = GT

type Position = (Int, Int)

-- | Pointwise addition
(.+.) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)

data Direction = North | South | East | West
  deriving (Eq, Show)

data Path = Open | Closed
  deriving (Eq, Show)

isOpen :: Path -> Bool
isOpen Open = True
isOpen Closed = False

data Edge e = Edge
  { nodeID :: NodeID,
    e :: Path
  }
  deriving (Show, Eq)

type Edges e = (Maybe (Edge e), Maybe (Edge e), Maybe (Edge e), Maybe (Edge e))

data Node a e = Node
  { nid :: NodeID,
    value :: a,
    north :: Maybe (Edge e),
    south :: Maybe (Edge e),
    east :: Maybe (Edge e),
    west :: Maybe (Edge e)
  }
  deriving (Show, Eq)

openConnections :: Node a Path -> [NodeID]
openConnections = pathsWith isOpen

closedConnections :: Node a Path -> [NodeID]
closedConnections = pathsWith (not . isOpen)

pathsWith :: (Path -> Bool) -> Node a Path -> [NodeID]
pathsWith p n = nodeID <$> filter (\(Edge _ b) -> p b) (paths n)

connections :: Node a Path -> [NodeID]
connections n = nodeID <$> paths n

connectionsWith :: (Edge e -> Bool) -> Node a e -> [NodeID]
connectionsWith p n = nodeID <$> filter p (paths n)

paths :: Node a e -> [Edge e]
paths (Node _ _ n s e w) = catMaybes [n, s, e, w]

nodeWithConnections :: Position -> a -> Edges e -> Node a e
nodeWithConnections position val (n, s, e, w) =
  Node
    { nid = NodeID position,
      value = val,
      north = n,
      south = s,
      east = e,
      west = w
    }

type QuadGraph a e = Map NodeID (Node a e)

type Maze = QuadGraph (Maybe Int) Path

type MazeNode = Node (Maybe Int) Path

emptyMaze :: Maze
emptyMaze = Map.empty

mazeToList :: Maze -> [MazeNode]
mazeToList = Map.elems

newMaze :: Width -> Maze
newMaze size =
  Map.fromList
    [ (NodeID (x, y), nodeWithConnections (x, y) Nothing (mkConnections size (x, y)))
      | y <- [0 .. size - 1],
        x <- [0 .. size - 1]
    ]

mkConnections :: Width -> Position -> Edges e
mkConnections width pos@(x, y) =
  ( if y < width - 1 then connection North else Nothing, -- north
    if y > 0 then connection South else Nothing, -- south
    if x < width - 1 then connection East else Nothing, -- east
    if x > 0 then connection West else Nothing -- west
  )
  where
    nid = NodeID pos
    connection n = Just $ Edge (directionNode nid n) Closed

connect :: NodeID -> NodeID -> Maze -> Maze
connect a b m = case nodesDirection a b of
  North -> connectN a b m
  South -> connectS a b m
  East -> connectE a b m
  West -> connectW a b m

-- | remove a node from second nodes adjacency list
remove :: NodeID -> NodeID -> Maze -> Maze
remove a b m = case nodesDirection a b of
  North -> removeN a b m
  South -> removeS a b m
  East -> removeE a b m
  West -> removeW a b m

directionNode :: NodeID -> Direction -> NodeID
directionNode (NodeID pos) North = NodeID $ pos .+. (0, 1)
directionNode (NodeID pos) South = NodeID $ pos .+. (0, -1)
directionNode (NodeID pos) East = NodeID $ pos .+. (1, 0)
directionNode (NodeID pos) West = NodeID $ pos .+. (-1, 0)

nodesDirection :: NodeID -> NodeID -> Direction
nodesDirection (NodeID (x1, y1)) (NodeID (x2, y2))
  | x1 == x2 && y1 + 1 == y2 = North
  | x1 == x2 && y1 - 1 == y2 = South
  | y1 == y2 && x1 + 1 == x2 = East
  | y1 == y2 && x1 - 1 == x2 = West
  | otherwise = error "can't determine direction between two nodes"

-- | Connect connects two nodes together and returns the
-- new maze with connected nodes
connectN :: NodeID -> NodeID -> Maze -> Maze
connectN a b m =
  Map.adjust (\x -> x {north = Just (Edge b Open)}) a $
    Map.adjust (\x -> x {south = Just (Edge a Open)}) b m

connectS :: NodeID -> NodeID -> Maze -> Maze
connectS a b m =
  do
    Map.adjust (\x -> x {south = Just (Edge b Open)}) a
    $ Map.adjust (\x -> x {north = Just (Edge a Open)}) b m

connectE :: NodeID -> NodeID -> Maze -> Maze
connectE a b m =
  do
    Map.adjust (\x -> x {east = Just (Edge b Open)}) a
    $ Map.adjust (\x -> x {west = Just (Edge a Open)}) b m

connectW :: NodeID -> NodeID -> Maze -> Maze
connectW a b m =
  do
    Map.adjust (\x -> x {west = Just (Edge b Open)}) a
    $ Map.adjust (\x -> x {east = Just (Edge a Open)}) b m

removeN :: NodeID -> NodeID -> Maze -> Maze
removeN a b m =
  Map.adjust (\x -> x {north = Nothing}) a $
    Map.adjust (\x -> x {south = Nothing}) b m

removeS :: NodeID -> NodeID -> Maze -> Maze
removeS a b m =
  do
    Map.adjust (\x -> x {south = Nothing}) a
    $ Map.adjust (\x -> x {north = Nothing}) b m

removeE :: NodeID -> NodeID -> Maze -> Maze
removeE a b m =
  do
    Map.adjust (\x -> x {east = Nothing}) a
    $ Map.adjust (\x -> x {west = Nothing}) b m

removeW :: NodeID -> NodeID -> Maze -> Maze
removeW a b m =
  do
    Map.adjust (\x -> x {west = Nothing}) a
    $ Map.adjust (\x -> x {east = Nothing}) b m
