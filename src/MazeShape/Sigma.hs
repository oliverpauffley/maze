{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Mazes with hexagonal shaped nodes.
module MazeShape.Sigma where

import Diagrams.Backend.SVG (B)
import Diagrams.Backend.SVG.CmdLine (mainWith)
import Diagrams.Prelude hiding (Direction, center)
import MazeShapeV2

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

{- | Hexagonal grids using the "offset" coordinate system
see https://www.redblobgames.com/grids/hexagons/#coordinates
-}
newtype Sigma = Sigma (Int, Int)
    deriving (Show, Eq, Ord)

instance GridShape Sigma where
    data Direction Sigma = North | South | NorthEast | NorthWest | SouthEast | SouthWest
        deriving (Show, Enum, Bounded)

    neighbor :: Sigma -> Direction Sigma -> Maybe Sigma
    neighbor (Sigma (q, r)) North = Just $ Sigma (q, r + 1)
    neighbor (Sigma (q, r)) South = Just $ Sigma (q, r - 1)
    neighbor (Sigma (q, r)) NorthEast = Just $ Sigma (q + 1, r)
    neighbor (Sigma (q, r)) NorthWest = Just $ Sigma (q - 1, r)
    neighbor (Sigma (q, r)) SouthEast = Just $ Sigma (q + 1, r + 1)
    neighbor (Sigma (q, r)) SouthWest = Just $ Sigma (q - 1, r - 1)

    toShape :: Sigma -> NodeShape (Direction Sigma)
    toShape (Sigma pos) = NodeShape point undefined
      where
        point = posToPoint pos

posToPoint :: (Int, Int) -> Point V2 Double
posToPoint (x, y)
    | odd x =
        (fromIntegral x * (xFarEast + xNearEast)) ^& (ySouth + fromIntegral y * yNorth * 2)
    | otherwise = (fromIntegral x * (xFarEast + xNearEast)) ^& (fromIntegral y * (yNorth * 2))

-- TODO this needs to be the same directions as the base sigma directions so we can use it OR do we add the directions as keys?
-- TODO looks like this doesn't tesselate right now. Needs fixing
-- >>> mconcat $ sigmaEdges (0 ^& 0)
sigmaEdges :: Point V2 Double -> [Located (Trail V2 Double)]
sigmaEdges c =
    map
        (fromVertices . (map (\p -> c ^+^ p)))
        [ [(xNearWest ^& yNorth), (xNearEast ^& yNorth)]
        , [(xNearEast ^& yNorth), (xFarEast ^& center)]
        , [(xFarEast ^& center), (xNearEast ^& ySouth)]
        , [(xNearEast ^& ySouth), (xNearWest ^& ySouth)]
        , [(xNearWest ^& ySouth), (xFarWest ^& center)]
        , [(xFarWest ^& center), (xNearWest ^& yNorth)]
        ]

testDiag :: Diagram B
testDiag =
    mconcat
        ( map
            stroke
            ((sigmaEdges $ posToPoint (1 ^& 1)) ++ (sigmaEdges $ posToPoint (0 ^& 0)) ++ (sigmaEdges $ posToPoint (1 ^& 2)))
        )

-- to test ghci> :main -o test.svg -w 400
main :: IO ()
main = mainWith testDiag

-- instance GridKind Sigma where
--     makeGrid = newHexagonalGrid

-- newHexagonalGrid :: (Rep d ~ SigmaDir, Representable d) => Int -> Map.Map NodeID (Node d (Maybe a) Path)
-- newHexagonalGrid w =
--     Map.fromList [(NodeID (x, y), mkNode (x, y)) | y <- [0 .. w - 1], x <- [0 .. w - 1]]
--   where
--     mkNode pos = Node (NodeID pos) Nothing (tabulate (mkPaths w pos))

-- mkPaths :: Int -> (Int, Int) -> SigmaDir -> MEdge Path
-- mkPaths w pos dir = do
--     (dx, dy) <- dirOffset pos dir
--     let (x', y') = pos .+. (dx, dy)
--     guard (x' >= 0 && y' >= 0 && x' < w && y' < w)
--     return (Edge (NodeID (x', y')) Closed)

-- directionNode :: NodeID -> SigmaDir -> NodeID
-- directionNode (NodeID pos) dir = NodeID $ pos .+. fromJust (dirOffset pos dir)

-- dirOffset :: (Int, Int) -> SigmaDir -> Maybe (Int, Int)
-- dirOffset (x, _) dir = case dir of
--     North -> Just (0, 1)
--     South -> Just (0, -1)
--     NorthEast -> Just (1, if odd x then 0 else 1)
--     SouthEast -> Just (1, if odd x then -1 else 0)
--     NorthWest -> Just (-1, if even x then 1 else 0)
--     SouthWest -> Just (-1, if even x then 0 else -1)
