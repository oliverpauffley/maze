{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module MazeShape (
    GridShape (..),
    nodeNeighbours,
    Maze (..),
    MazeBuilder,
    runBuilder,
    NorthEastDirection (..),
    connectEdge,
    getNorthEastNeighbors,
    mazeNodes,
    mazeEdges,
    EdgeState (..),
    edgeKey,
    getEdgeState,
    getNorth,
    allCoords,
    randomNode,
    getEdgesWith,
    getEdges,
    getClosedEdges,
    getOpenEdges,
    NodeShape (..),
    edges,
    Config (..),
    Algorithm (..),
    Shape (..),
    getNodeValue,
    center,
)
where

import Control.Lens
import Control.Monad.Random (MonadRandom, uniform)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT))
import Data.Foldable (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust)
import qualified Debug.Trace as Debug
import Diagrams (Point, V2)
import Diagrams.Located (Located)
import Diagrams.Prelude (Trail)

data Config = Config
    { diagramSize :: Double
    , mazeSize :: Int
    , solve :: Bool
    , withColor :: Bool
    , countDeadEnds :: Bool
    , debug :: Bool
    , fileName :: String
    , -- TODO fix mask
      -- , mask :: Maybe FilePath
      algorithm :: Algorithm
    , shape :: Shape
    }

data Algorithm
    = BinaryTree
    | Sidewinder
    | AldousBroder
    | Wilson
    | HuntKill
    | RecursiveBacktrack
    deriving (Eq, Show)

data Shape
    = ShapeSquare
    | ShapeHexagon
    deriving (Eq, Show)

data NodeShape direction = NodeShape
    { _center :: Point V2 Double
    , _edges :: Map direction (Located (Trail V2 Double))
    {- ^ the edges are the possible edges that could be drawn
     we don't know if these should be drawn until we check the mazeEdges field
    -}
    }

makeLenses ''NodeShape

class GridShape coord where
    -- | Each grid shape has it's own coordinate system that has directions between each coordinate pair.
    data Direction coord

    {- | Get all neighbouring coordinates to the given starting point.
    | It is not guaranteed that these are within a given maze since that depends on it's size.
    | for example at coord (0,0) this neighbors will return (-1,-1) which is not a valid coordinate
    -}
    neighbors :: coord -> [coord]

    -- |  if the direction is bounded we can provide a default implementation
    default neighbors ::
        (Bounded (Direction coord), Enum (Direction coord)) =>
        coord -> [coord]
    neighbors c = catMaybes [neighbor c d | d <- [minBound .. maxBound]]

    {- | Given a starting point and direction find the possible coordinate.
    | it is possible in some cases that you might not get a new coordinate (for example in triangluar shaped mazes where adjacent triangles are flipped)
    -}
    neighbor :: coord -> Direction coord -> Maybe coord

    -- | convert a coordinate to a point in 2D space.
    toShape :: coord -> NodeShape (Direction coord)

-- | some coordinate systems implement a set of directions that could be casted to north and east.
class NorthEastDirection d where
    northDir :: d
    eastDir :: d

getNorth ::
    (GridShape c, NorthEastDirection (Direction c)) =>
    c -> Maybe c
getNorth c = neighbor c northDir

getEast ::
    (GridShape c, NorthEastDirection (Direction c)) =>
    c -> Maybe c
getEast c = neighbor c eastDir

data EdgeState = Open | Closed
    deriving (Show, Eq)

isOpen :: EdgeState -> Bool
isOpen Open = True
isOpen _ = False

isClosed :: EdgeState -> Bool
isClosed = not . isOpen

data Maze coord nodeData = Maze
    { _mazeNodes :: Map coord nodeData
    , -- Edges are stored as a Map of coordinate pairs to their state.
      -- the first coord in the pair is smallest so that we don't store from both directions
      -- e.g. ((0,1), (1,1)) would be in the map but not ((1,1), (0,1)).
      _mazeEdges :: Map (coord, coord) EdgeState
    }
    deriving (Show, Eq)

makeLenses ''Maze

-- | Return all coordinates in the maze.
allCoords :: Maze coord nodeData -> [coord]
allCoords maze = Map.keys $ maze ^. mazeNodes

-- | Select a random node from anywhere within the maze.
randomNode :: (MonadRandom m) => Maze coord a -> m coord
randomNode m = uniform $ allCoords m

instance Functor (Maze coord) where
    fmap :: (a -> b) -> Maze coord a -> Maze coord b
    fmap f (Maze ns es) = Maze (f <$> ns) es

getNodeValue :: (Ord coord) => coord -> Maze coord a -> Maybe a
getNodeValue c (Maze ns _) = Map.lookup c ns

isNode :: (Ord coord) => Maze coord a -> coord -> Bool
isNode m c = isJust $ getNodeValue c m

getNorthEastNeighbors ::
    (GridShape c, NorthEastDirection (Direction c), Ord c) =>
    Maze c a ->
    c ->
    (Maybe c, Maybe c)
getNorthEastNeighbors maze c =
    let (n, e) = (getNorth c, getEast c)
     in (find (isNode maze) n, find (isNode maze) e)

{- | Standardizes an edge pair so (A, B) and (B, A) result in the same key.
all connections/data is stored with smallest value first.
-}
edgeKey :: (Ord coord) => coord -> coord -> (coord, coord)
edgeKey c1 c2
    | c1 <= c2 = (c1, c2)
    | otherwise = (c2, c1)

-- >>> getEdgeState 1 2 (Maze Map.empty (Map.fromList [((1,2), Closed)]))
-- Just Closed
getEdgeState :: (Ord coord) => coord -> coord -> Maze coord a -> Maybe EdgeState
getEdgeState u v maze =
    maze ^. mazeEdges . at (edgeKey u v)

-- >>> setEdgeState 2 1 Open (Maze Map.empty (Map.fromList [((1,2), Closed)]))
-- Maze {_mazeNodes = fromList [], _mazeEdges = fromList [((1,2),Open)]}
setEdgeState :: (Ord coord) => coord -> coord -> EdgeState -> Maze coord nodeData -> Maze coord nodeData
setEdgeState u v state maze =
    maze & mazeEdges . at (edgeKey u v) ?~ state

-- >>> connectEdge 2 1 (Maze Map.empty (Map.fromList [((1,2), Closed)]))
-- Maze {_mazeNodes = fromList [], _mazeEdges = fromList [((1,2),Open)]}
connectEdge :: (Ord coord, Show coord) => coord -> coord -> Maze coord a -> Maze coord a
connectEdge u v maze =
    if not (isNode maze u) || not (isNode maze v)
        then Debug.traceShow (u, v) error "connecting invalid node "
        else setEdgeState u v Open maze

getEdges :: (Ord coord, GridShape coord) => coord -> Maze coord a -> [(coord, EdgeState)]
getEdges c maze = getEdges' c maze (const True)

nodeNeighbours :: (Ord coord, GridShape coord) => coord -> Maze coord a -> [coord]
nodeNeighbours c maze = map fst $ getEdges c maze

getEdges' ::
    (Ord coord, GridShape coord) => coord -> Maze coord a -> (EdgeState -> Bool) -> [(coord, EdgeState)]
getEdges' c maze f =
    let
        ns = filter (isNode maze) $ neighbors c
     in
        foldl' go [] ns
  where
    go acc c' = case getEdgeState c c' maze of
        Just e | f e -> (c', e) : acc
        _otherwise -> acc

getEdgesWith :: (Ord coord, GridShape coord) => coord -> (coord -> Bool) -> Maze coord a -> [coord]
getEdgesWith c f maze = filter f $ map fst $ getEdges c maze

getOpenEdges :: (Ord coord, GridShape coord) => coord -> Maze coord a -> [coord]
getOpenEdges c maze = fst <$> getEdges' c maze isOpen

getClosedEdges :: (Ord coord, GridShape coord) => coord -> Maze coord a -> [coord]
getClosedEdges c maze = fst <$> getEdges' c maze isClosed

-- | The main monad for the generate of mazes
type MazeBuilder s a = ReaderT Config (StateT s IO) a

-- | Run the builder to produce a maze
runBuilder :: MazeBuilder state a -> Config -> state -> IO (a, state)
runBuilder app c s = do
    (a, s') <- runStateT (runReaderT app c) s
    return (a, s')
