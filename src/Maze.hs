module Maze where

import           Control.Monad.Trans.State.Strict (StateT, modify)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map

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

data Direction = North | South | East | West
  deriving (Eq, Show)

data Path = Open | Closed
  deriving (Eq, Show)

data Edge e = Edge
  { nodeID :: NodeID,
    e      :: Path
  }
  deriving (Show, Eq)

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
  deriving (Show, Eq)

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

type Maze = QuadGraph () Path

type MazeNode = Node () Path

emptyMaze :: Maze
emptyMaze = Map.empty

mazeToList :: Maze -> [Node () Path]
mazeToList = Map.elems

newMaze :: Width -> Maze
newMaze size =
  Map.fromList
    [ (NodeID (x, y), nodeWithConnections (x, y) () (connections size (x, y)))
      | y <- [0 .. size - 1],
        x <- [0 .. size - 1]
    ]

connections :: Width -> Position -> Edges e
connections width (x, y) =
  ( if y < width - 1 then Just $ Edge (NodeID (x, y + 1)) Closed else Nothing, -- north
    if y > 0 then Just $ Edge (NodeID (x, y - 1)) Closed else Nothing, -- south
    if x < width - 1 then Just $ Edge (NodeID (x + 1, y)) Closed else Nothing, -- east
    if x > 0 then Just $ Edge (NodeID (x - 1, y)) Closed else Nothing -- west
  )

connect :: NodeID -> NodeID -> Maze -> Maze
connect a b m = case nodesDirection a b of
  North -> connectN a b m
  South -> connectS a b m
  East  -> connectE a b m
  West  -> connectW a b m

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
