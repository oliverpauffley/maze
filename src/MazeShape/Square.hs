{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

-- | Mazes with square nodes. Movement is based on the cardinal north, south, east, west directions.
module MazeShape.Square where

import Data.Map (Map)
import qualified Data.Map as Map
import Diagrams.Prelude hiding (Direction, Path, center, index, value)
import MazeShape

newtype Square = Square (Int, Int)
    deriving (Show, Eq, Ord)

instance GridShape Square where
    data Direction Square = North | South | East | West
        deriving (Show, Enum, Bounded, Eq, Ord)

    neighbor :: Square -> Direction Square -> Maybe Square
    neighbor (Square (x, y)) North = Just $ Square (x, y - 1)
    neighbor (Square (x, y)) South = Just $ Square (x, y + 1)
    neighbor (Square (x, y)) East = Just $ Square (x + 1, y)
    neighbor (Square (x, y)) West = Just $ Square (x - 1, y)

    toShape :: Square -> NodeShape (Direction Square)
    toShape (Square (x, y)) = NodeShape point $ squareEdges point
      where
        point = fromIntegral x ^& fromIntegral (-y)

squareLength :: (Num a) => a
squareLength = 1

center = 0

minS = center - 0.5 * squareLength
maxS = center + 0.5 * squareLength

squareEdges :: Point V2 Double -> Map (Direction Square) (Located (Trail V2 Double))
squareEdges c =
    Map.fromList $
        zip
            [North, South, East, West]
            ( map
                (fromVertices . (map (\p -> c ^+^ p)))
                [ [(minS ^& maxS), (maxS ^& maxS)]
                , [(minS ^& minS), (maxS ^& minS)]
                , [(minS ^& minS), (minS ^& maxS)]
                , [(maxS ^& minS), (maxS ^& maxS)]
                ]
            )

-- >>> newSquareGrid 2
-- Maze {_mazeNodes = fromList [(Square (0,0),()),(Square (0,1),()),(Square (1,0),()),(Square (1,1),())], _mazeEdges = fromList [((Square (0,0),Square (0,1)),Closed),((Square (0,0),Square (1,0)),Closed),((Square (0,1),Square (1,1)),Closed),((Square (1,0),Square (1,1)),Closed)]}
newSquareGrid :: Int -> Maze Square ()
newSquareGrid s =
    Maze
        ( Map.fromList $
            map
                (\ns -> () <$ ns)
                nodes
        )
        ( Map.fromList $
            concatMap
                (\(n, ns) -> [((n, n'), Closed) | n' <- ns])
                nodes
        )
  where
    nodes =
        [ (n, toConnect)
        | x <- [0 .. s - 1]
        , y <- [0 .. s - 1]
        , let n = Square (x, y)
        , let toConnect = filter (\n' -> n < n' && inbounds s n') $ neighbors n
        ]

-- | returns true if the given coordinate is within the shape size.
inbounds :: Int -> Square -> Bool
inbounds size (Square (x, y)) = x >= 0 && y >= 0 && x < size && y < size
