module Maze where

import           Data.Map (Map)
import qualified Data.Map as Map

-- all mazes are square so width is height too
type Width = Int

type NodeID = (Int, Int)

type Position = (Int, Int)

data Direction = North | South | East | West
  deriving (Eq, Show)

data Path = Open | Closed
  deriving (Eq, Show)

data Edge e = Edge NodeID e
  deriving (Show)

-- | returns the ID of the connected Node.
edgeToNodeID :: Edge e -> NodeID
edgeToNodeID (Edge nodeId _) = nodeId

type Edges e = (Maybe (Edge e), Maybe (Edge e), Maybe (Edge e), Maybe (Edge e))

data Node a e = Node
  { nid   :: NodeID,
    value :: a,
    north :: Maybe (Edge e),
    south :: Maybe (Edge e),
    east  :: Maybe (Edge e),
    west  :: Maybe (Edge e)
  }
  deriving (Show)

nodeWithConnections :: NodeID -> a -> (Maybe (Edge e), Maybe (Edge e), Maybe (Edge e), Maybe (Edge e)) -> Node a e
nodeWithConnections nodeID val (n, s, e, w) =
  Node
    { nid = nodeID,
      value = val,
      north = n,
      south = s,
      east = e,
      west = w
    }

type QuadGraph a e = Map NodeID (Node a e)

insert :: QuadGraph a e -> NodeID -> a -> QuadGraph a e
insert graph nodeID val =
  Map.insert
    nodeID
    Node
      { nid = nodeID,
        value = val,
        north = Nothing,
        south = Nothing,
        east = Nothing,
        west = Nothing
      }
    graph

type Maze = QuadGraph () Path

type MazeNode = Node () Path

emptyMaze :: Maze
emptyMaze = Map.empty

mazeToList :: Maze -> [Node () Path]
mazeToList = Map.elems

newMaze :: Width -> Maze
newMaze size =
  Map.fromList
    [ ((x, y), nodeWithConnections (x, y) () (connections size (x, y)))
      | x <- [0 .. size - 1],
        y <- [0 .. size - 1]
    ]

connections :: Width -> Position -> (Maybe (Edge Path), Maybe (Edge Path), Maybe (Edge Path), Maybe (Edge Path))
connections width (x, y) =
  ( if y < width - 1 then Just $ Edge (x, y + 1) Closed else Nothing, -- north
    if y > 0 then Just $ Edge (x, y - 1) Closed else Nothing, -- south
    if x < width - 1 then Just $ Edge (x + 1, y) Closed else Nothing, -- east
    if x < 0 then Just $ Edge (x - 1, y) Closed else Nothing -- west
  )

connect :: Maze -> NodeID -> NodeID -> Maze
connect m a b = case nodesDirection a b of
  North -> connectN m a b
  South -> connectS m a b
  East  -> connectE m a b
  West  -> connectW m a b

nodesDirection :: NodeID -> NodeID -> Direction
nodesDirection (x1, y1) (x2, y2)
  | x1 == x2 && y1 + 1 == y2 = North
  | x1 == x2 && y1 - 1 == y2 = South
  | y1 == y2 && x1 + 1 == x2 = East
  | y1 == y2 && x1 - 1 == x2 = West

-- | Connect connects two nodes together and returns the
-- new maze with connected nodes
connectN :: Maze -> NodeID -> NodeID -> Maze
connectN m a b =
  Map.adjust (\x -> x {north = Just (Edge b Open)}) a $
    Map.adjust (\x -> x {south = Just (Edge a Open)}) b m

connectS :: Maze -> NodeID -> NodeID -> Maze
connectS m a b =
  Map.adjust (\x -> x {south = Just (Edge b Open)}) a $
    Map.adjust (\x -> x {north = Just (Edge a Open)}) b m

connectE :: Maze -> NodeID -> NodeID -> Maze
connectE m a b =
  Map.adjust (\x -> x {east = Just (Edge b Open)}) a $
    Map.adjust (\x -> x {west = Just (Edge a Open)}) b m

connectW :: Maze -> NodeID -> NodeID -> Maze
connectW m a b =
  Map.adjust (\x -> x {west = Just (Edge b Open)}) a $
    Map.adjust (\x -> x {east = Just (Edge a Open)}) b m
