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
import qualified MazeShape.Square as MSS

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

data SigmaDir = North | NorthWest | SouthWest | South | SouthEast | NorthEast
    deriving (Enum, Eq, Ord, Bounded, Show)

instance Opposite SigmaDir where
    opposite :: SigmaDir -> SigmaDir
    opposite North = South
    opposite South = North
    opposite NorthWest = SouthEast
    opposite SouthEast = NorthWest
    opposite NorthEast = SouthWest
    opposite SouthWest = NorthEast

instance MSS.FromCardinalDir SigmaDir where
    fromCardinalDir MSS.North = North
    fromCardinalDir MSS.South = South
    fromCardinalDir MSS.East = SouthEast
    fromCardinalDir MSS.West = NorthWest

data Sigma a = Sigma
    { _north :: a
    , _northWest :: a
    , _southWest :: a
    , _south :: a
    , _southEast :: a
    , _northEast :: a
    }

makeLenses ''Sigma

instance Functor Sigma where
    fmap :: (a -> b) -> Sigma a -> Sigma b
    fmap f (Sigma n nw sw s se ne) = Sigma (f n) (f nw) (f sw) (f s) (f se) (f ne)

instance Distributive Sigma where
    distribute :: (Functor f) => f (Sigma a) -> Sigma (f a)
    distribute m =
        Sigma
            (view north <$> m)
            (view northWest <$> m)
            (view southWest <$> m)
            (view south <$> m)
            (view southEast <$> m)
            (view northEast <$> m)

instance Representable Sigma where
    type Rep Sigma = SigmaDir

    index :: Sigma a -> (Rep Sigma -> a)
    index (Sigma n _ _ _ _ _) North = n
    index (Sigma _ nw _ _ _ _) NorthWest = nw
    index (Sigma _ _ sw _ _ _) SouthWest = sw
    index (Sigma _ _ _ s _ _) South = s
    index (Sigma _ _ _ _ se _) SouthEast = se
    index (Sigma _ _ _ _ _ ne) NorthEast = ne

    tabulate :: (Rep Sigma -> a) -> Sigma a
    tabulate b = Sigma (b North) (b NorthWest) (b SouthWest) (b South) (b SouthEast) (b NorthEast)

instance Applicative Sigma where
    pure :: a -> Sigma a
    pure a = Sigma a a a a a a
    (<*>) :: Sigma (a -> b) -> Sigma a -> Sigma b
    (<*>) (Sigma f1 f2 f3 f4 f5 f6) (Sigma n nw sw s se ne) =
        Sigma (f1 n) (f2 nw) (f3 sw) (f4 s) (f5 se) (f6 ne)

instance DrawMaze Sigma where
    nodeToPoint :: Node Sigma v p -> Point V2 Double
    nodeToPoint (Node (NodeID (x, y)) _ _)
        | odd x = (fromIntegral x * xFarEast + xNearEast) ^& (-ySouth + fromIntegral y * yNorth * 2)
        | otherwise = (fromIntegral x * xFarEast * 2 + lineS) ^& (fromIntegral y * yNorth * 2)

    drawEdges :: Sigma (MEdge Path) -> [(MEdge Path, [Point V2 Double])]
    drawEdges = drawSigmaEdges

    colorNode :: Colour Double -> Maybe Int -> MazeBuilder (Maze Sigma) (Diagram B)
    colorNode colour ma = do
        Config{..} <- ask
        guard withColor
        mx <- gets (fromIntegral . maxValue)
        return $ colorN ma mx colour
      where
        colorN :: Maybe Int -> Double -> Colour Double -> Diagram B
        colorN Nothing _ _ = shape # fc black lw 0
        colorN (Just a) mx rCol = shape # fcA (col rCol ((mx - fromIntegral a) / mx)) lw none

        shape = polygon (with & polyType .~ PolyRegular 6 xFarEast)

        col c v = toAlphaColour $ blend v c black

        maxValue m =
            fromJust . _value $ maximumBy (compare `on` _value) (Map.elems m)

drawSigmaEdges :: Sigma (MEdge Path) -> [(MEdge Path, [Point V2 Double])]
drawSigmaEdges (Sigma n nw sw s se ne) =
    [ (n, map p2 [(xNearWest, yNorth), (xNearEast, yNorth)])
    , (nw, map p2 [(xNearWest, yNorth), (xFarWest, center)])
    , (sw, map p2 [(xFarWest, center), (xNearWest, ySouth)])
    , (s, map p2 [(xNearWest, ySouth), (xNearEast, ySouth)])
    , (se, map p2 [(xNearEast, ySouth), (xFarEast, center)])
    , (ne, map p2 [(xFarEast, center), (xNearEast, yNorth)])
    ]

newHexagonalGrid :: (Rep d ~ SigmaDir, Representable d) => Int -> Map.Map NodeID (Node d (Maybe a) Path)
newHexagonalGrid w =
    Map.fromList [(NodeID (x, y), mkNode (x, y)) | y <- [0 .. w - 1], x <- [0 .. w - 1]]
  where
    mkNode pos = Node (NodeID pos) Nothing (tabulate (mkPaths w pos))

mkPaths :: Int -> (Int, Int) -> SigmaDir -> MEdge Path
mkPaths w pos@(x, y) dir =
    guard (test dir)
        >> Just (Edge (directionNode (NodeID pos) North) Closed)
  where
    test North = y < w - 1
    test South = y > 0
    test NorthEast = x < w - 1 && y < w - 1
    test SouthEast = x < w - 1
    test NorthWest = x > 0
    test SouthWest = x > 0 && y > 0

directionNode :: NodeID -> SigmaDir -> NodeID
directionNode (NodeID pos) North = NodeID $ pos .+. (0, 1)
directionNode (NodeID pos) South = NodeID $ pos .+. (0, -1)
directionNode (NodeID pos) NorthEast = NodeID $ pos .+. (1, 1)
directionNode (NodeID pos) SouthEast = NodeID $ pos .+. (1, 0)
directionNode (NodeID pos) NorthWest = NodeID $ pos .+. (-1, 0)
directionNode (NodeID pos) SouthWest = NodeID $ pos .+. (-1, -1)
