{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Mazes with hexagonal shaped nodes.
module MazeShape.Sigma where

import Control.Monad (guard)
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.State (gets)
import Data.Distributive (Distributive (distribute))
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Functor.Rep
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude hiding (Path, center)
import Draw (DrawMaze (..))
import GridKind (GridKind)
import qualified GridKind as GK
import MazeShape (
    Config (..),
    Edge (..),
    MEdge,
    Maze,
    MazeBuilder,
    Node (..),
    NodeID (..),
    Opposite (opposite),
    Path (..),
    (.+.),
 )

lineS :: Double
lineS = 1

halfS :: Double
halfS = lineS / 2

halfHeight :: Double
halfHeight = lineS * sqrt 3 / 2

center :: Double
center = 0

xFarWest :: Double
xFarWest = center - lineS

xFarEast :: Double
xFarEast = center + lineS

xNearWest :: Double
xNearWest = center - halfS

xNearEast :: Double
xNearEast = center + halfS

yNorth :: Double
yNorth = center + halfHeight

ySouth :: Double
ySouth = center - halfHeight

data SigmaDir = North | NorthEast | SouthEast | South | SouthWest | NorthWest
    deriving (Enum, Eq, Ord, Bounded, Show)

instance Opposite SigmaDir where
    opposite :: SigmaDir -> SigmaDir
    opposite North = South
    opposite South = North
    opposite NorthEast = SouthWest
    opposite SouthWest = NorthEast
    opposite NorthWest = SouthEast
    opposite SouthEast = NorthWest

instance GK.FromCardinalDir SigmaDir where
    fromCardinalDir GK.North = North
    fromCardinalDir GK.South = South
    fromCardinalDir GK.East = SouthEast
    fromCardinalDir GK.West = NorthWest

data Sigma a = Sigma
    { _north :: a
    , _northEast :: a
    , _southEast :: a
    , _south :: a
    , _southWest :: a
    , _northWest :: a
    }
    deriving (Show)

makeLenses ''Sigma

instance Functor Sigma where
    fmap :: (a -> b) -> Sigma a -> Sigma b
    fmap f (Sigma n ne se s sw nw) = Sigma (f n) (f ne) (f se) (f s) (f sw) (f nw)

instance Distributive Sigma where
    distribute :: (Functor f) => f (Sigma a) -> Sigma (f a)
    distribute m =
        Sigma
            (view north <$> m)
            (view northEast <$> m)
            (view southEast <$> m)
            (view south <$> m)
            (view southWest <$> m)
            (view northWest <$> m)

instance Representable Sigma where
    type Rep Sigma = SigmaDir

    index :: Sigma a -> (Rep Sigma -> a)
    index s North = s ^. north
    index s NorthEast = s ^. northEast
    index s SouthEast = s ^. southEast
    index s South = s ^. south
    index s SouthWest = s ^. southWest
    index s NorthWest = s ^. northWest

    tabulate :: (Rep Sigma -> a) -> Sigma a
    tabulate b = Sigma (b North) (b NorthEast) (b SouthEast) (b South) (b SouthWest) (b NorthWest)

instance Applicative Sigma where
    pure :: a -> Sigma a
    pure a = Sigma a a a a a a
    (<*>) :: Sigma (a -> b) -> Sigma a -> Sigma b
    (<*>) (Sigma f1 f2 f3 f4 f5 f6) (Sigma n ne se s sw nw) =
        Sigma (f1 n) (f2 ne) (f3 se) (f4 s) (f5 sw) (f6 nw)

instance DrawMaze Sigma where
    nodeToPoint :: Node Sigma v p -> Point V2 Double
    nodeToPoint (Node (NodeID pos) _ _) = posToPoint pos
      where
        posToPoint :: (Int, Int) -> Point V2 Double
        posToPoint (x, y)
            | odd x =
                (fromIntegral x * (xFarEast + xNearEast)) ^& (ySouth + fromIntegral y * yNorth * 2)
            | otherwise = (fromIntegral x * (xFarEast + xNearEast)) ^& (fromIntegral y * (yNorth * 2))

    drawEdges :: Sigma (MEdge Path) -> [(MEdge Path, [Point V2 Double])]
    drawEdges = drawSigmaEdges

    -- TODO this can be pulled apart for reuse across node shapes
    colorNode :: Colour Double -> Maybe Int -> MazeBuilder (Maze Sigma) (Diagram B)
    colorNode colour ma = do
        Config{..} <- ask
        if not withColor
            then
                return mempty
            else do
                mx <- gets (fromIntegral . maxValue)
                return $ colorN ma mx colour
      where
        colorN :: Maybe Int -> Double -> Colour Double -> Diagram B
        colorN Nothing _ _ = shape # fc grey lw 0
        colorN (Just a) mx rCol = shape # fcA (col rCol ((mx - fromIntegral a) / mx)) lw none

        shape = polygon (with & polyType .~ PolyRegular 6 xFarEast)

        col c v = toAlphaColour $ blend v c grey

        maxValue m =
            fromJust . _value $ maximumBy (compare `on` _value) (Map.elems m)

drawSigmaEdges :: Sigma (MEdge Path) -> [(MEdge Path, [Point V2 Double])]
drawSigmaEdges (Sigma n ne se s sw nw) =
    [ (n, map p2 [(xNearWest, yNorth), (xNearEast, yNorth)])
    , (ne, map p2 [(xNearEast, yNorth), (xFarEast, center)])
    , (se, map p2 [(xFarEast, center), (xNearEast, ySouth)])
    , (s, map p2 [(xNearEast, ySouth), (xNearWest, ySouth)])
    , (sw, map p2 [(xNearWest, ySouth), (xFarWest, center)])
    , (nw, map p2 [(xFarWest, center), (xNearWest, yNorth)])
    ]

instance GridKind Sigma where
    makeGrid = newHexagonalGrid

newHexagonalGrid :: (Rep d ~ SigmaDir, Representable d) => Int -> Map.Map NodeID (Node d (Maybe a) Path)
newHexagonalGrid w =
    Map.fromList [(NodeID (x, y), mkNode (x, y)) | y <- [0 .. w - 1], x <- [0 .. w - 1]]
  where
    mkNode pos = Node (NodeID pos) Nothing (tabulate (mkPaths w pos))

mkPaths :: Int -> (Int, Int) -> SigmaDir -> MEdge Path
mkPaths w pos dir = do
    (dx, dy) <- dirOffset pos dir
    let (x', y') = pos .+. (dx, dy)
    guard (x' >= 0 && y' >= 0 && x' < w && y' < w)
    return (Edge (NodeID (x', y')) Closed)

directionNode :: NodeID -> SigmaDir -> NodeID
directionNode (NodeID pos) dir = NodeID $ pos .+. fromJust (dirOffset pos dir)

dirOffset :: (Int, Int) -> SigmaDir -> Maybe (Int, Int)
dirOffset (x, _) dir = case dir of
    North -> Just (0, 1)
    South -> Just (0, -1)
    NorthEast -> Just (1, if odd x then 0 else 1)
    SouthEast -> Just (1, if odd x then -1 else 0)
    NorthWest -> Just (-1, if even x then 1 else 0)
    SouthWest -> Just (-1, if even x then 0 else -1)
