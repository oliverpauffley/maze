module Maze (newMaze, Maze) where

import           Data.Map (Map)
import qualified Data.Map as Map

-- all mazes are square so width is height too
type Width = Int

type NodeID = (Int, Int)

type Position = (Int, Int)

data Path = Open | Closed
  deriving (Eq, Show)

data Edge e = Edge NodeID e
  deriving (Show)

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

emptyMaze :: Maze
emptyMaze = Map.empty

-- >>> newMaze 2
-- fromList [((0,0),Node {nid = (0,0), value = (), north = Nothing, south = Just (Edge (1,0) Closed), east = Just (Edge (0,1) Closed), west = Nothing}),((0,1),Node {nid = (0,1), value = (), north = Nothing, south = Just (Edge (1,1) Closed), east = Nothing, west = Just (Edge (0,0) Closed)}),((1,0),Node {nid = (1,0), value = (), north = Just (Edge (0,0) Closed), south = Nothing, east = Just (Edge (1,1) Closed), west = Nothing}),((1,1),Node {nid = (1,1), value = (), north = Just (Edge (0,1) Closed), south = Nothing, east = Nothing, west = Just (Edge (1,0) Closed)})]
newMaze :: Width -> Maze
newMaze size =
  Map.fromList
    [ ((x, y), nodeWithConnections (x, y) () (connections size (x, y)))
      | x <- [0 .. size - 1],
        y <- [0 .. size - 1]
    ]

connections :: Width -> Position -> (Maybe (Edge Path), Maybe (Edge Path), Maybe (Edge Path), Maybe (Edge Path))
connections width (x, y) =
  ( if x > 0 then Just $ Edge (x - 1, y) Closed else Nothing,
    if x < width - 1 then Just $ Edge (x + 1, y) Closed else Nothing,
    if y < width - 1 then Just $ Edge (x, y + 1) Closed else Nothing,
    if y > 0 then Just $ Edge (x, y - 1) Closed else Nothing
  )
