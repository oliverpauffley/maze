{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Mazes with square nodes. Movement is based on the cardinal north, south, east, west directions.
module MazeShape.Square where

import Data.Foldable (maximumBy)

import Control.Monad (guard)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, gets)
import Data.Distributive (Distributive (distribute))
import Data.Function (on)
import Data.Functor (($>))
import Data.Functor.Rep (Representable (Rep, index, tabulate))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude hiding (Direction, Path, center, index, value)
import Draw
import GridKind
import MazeShapeV2

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

-- instance ((Rep Cardinal ~ CardinalDir), Representable Cardinal) => GridKind Cardinal where
--     makeGrid = newSquareGrid

-- newSquareGrid :: (Rep d ~ CardinalDir, Representable d) => Int -> Map.Map NodeID (Node d (Maybe a) Path)
-- newSquareGrid w =
--     Map.fromList
--         [(NodeID (x, y), mkNode (x, y)) | y <- [0 .. w - 1], x <- [0 .. w - 1]]
--   where
--     mkNode pos = Node (NodeID pos) Nothing (tabulate (mkPaths w pos))

-- mkPaths :: Int -> (Int, Int) -> CardinalDir -> MEdge Path
-- mkPaths w pos@(_, y) North = newConnection (y < w - 1) (NodeID pos) North
-- mkPaths _ pos@(_, y) South = newConnection (y > 0) (NodeID pos) South
-- mkPaths w pos@(x, _) East = newConnection (x < w - 1) (NodeID pos) East
-- mkPaths _ pos@(x, _) West = newConnection (x > 0) (NodeID pos) West

-- newConnection :: Bool -> NodeID -> CardinalDir -> MEdge Path
-- newConnection False _ _ = Nothing
-- newConnection True nid dir = Just $ Edge (directionNode nid dir) Closed

-- directionNode :: NodeID -> CardinalDir -> NodeID
-- directionNode (NodeID pos) North = NodeID $ pos .+. (0, 1)
-- directionNode (NodeID pos) South = NodeID $ pos .+. (0, -1)
-- directionNode (NodeID pos) East = NodeID $ pos .+. (1, 0)
-- directionNode (NodeID pos) West = NodeID $ pos .+. (-1, 0)

-- instance Opposite CardinalDir where
--     opposite :: CardinalDir -> CardinalDir
--     opposite North = South
--     opposite South = North
--     opposite East = West
--     opposite West = East

-- instance FromCardinalDir CardinalDir where
--     fromCardinalDir = id

-- data Cardinal a = Cardinal
--     { _north :: a
--     , _east :: a
--     , _south :: a
--     , _west :: a
--     }
--     deriving (Eq, Show)

-- newtype SquareNode = SquareNode (Int, Int)
--     deriving (Show, Eq, Ord)

-- instance GridShape SquareNode where
--     type Direction SquareNode = CardinalDir

--     neighbors pos = catMaybes [neighbor pos b | b <- [minBound .. maxBound]]

--     neighbor (SquareNode pos) North = Just $ SquareNode $ (pos .+. (0, 1))
--     neighbor (SquareNode pos) South = Just $ SquareNode $ (pos .+. (0, -1))
--     neighbor (SquareNode pos) East = Just $ SquareNode $ (pos .+. (0, -1))
--     neighbor (SquareNode pos) West = Just $ SquareNode $ (pos .+. (0, -1))

-- makeLenses ''Cardinal

-- instance Functor Cardinal where
--     fmap f (Cardinal n e s w) = Cardinal (f n) (f e) (f s) (f w)

-- -- >>> distribute (Just (Cardinal 1 2 3 4))
-- -- Cardinal {_north = Just 1, _east = Just 2, _south = Just 3, _west = Just 4}
-- instance Distributive Cardinal where
--     distribute m = Cardinal (view north <$> m) (view east <$> m) (view south <$> m) (view west <$> m)

-- -- >>> index (Cardinal 1 2 3 4) South
-- -- 3
-- -- >>> tabulate (const 1) :: Cardinal Int
-- -- Cardinal {_north = 2, _east = 2, _south = 2, _west = 2}
-- instance Representable Cardinal where
--     type Rep Cardinal = CardinalDir

--     index :: Cardinal a -> (Rep Cardinal -> a)
--     index c North = c ^. north
--     index c East = c ^. east
--     index c South = c ^. south
--     index c West = c ^. west

--     tabulate :: (Rep Cardinal -> a) -> Cardinal a
--     tabulate c = Cardinal (c North) (c East) (c South) (c West)

-- instance Applicative Cardinal where
--     pure :: a -> Cardinal a
--     pure a = Cardinal a a a a

--     (<*>) :: Cardinal (a -> b) -> Cardinal a -> Cardinal b
--     (<*>) (Cardinal f1 f2 f3 f4) (Cardinal a b c d) = Cardinal (f1 a) (f2 b) (f3 c) (f4 d)

-- -- | for any representable type with an index that can be cast to a `Cardinal` direction, get the north and east components
-- northEastDirections :: (FromCardinalDir (Rep d), Representable d) => Node d a e -> [Maybe (Rep d, Edge e)]
-- northEastDirections n = [go North, go East]
--   where
--     go d = (fromCardinalDir d,) <$> index (n ^. directions) (fromCardinalDir d)

-- instance DrawMaze Cardinal where
--     drawEdges (Cardinal n e s w) =
--         [ (n, map p2 [(mHalfS, halfS), (halfS, halfS)])
--         , (s, map p2 [(mHalfS, mHalfS), (halfS, mHalfS)])
--         , (e, map p2 [(halfS, mHalfS), (halfS, halfS)])
--         , (w, map p2 [(mHalfS, mHalfS), (mHalfS, halfS)])
--         ]
--       where
--         side = 1.0
--         halfS = side / 2
--         mHalfS = -(side / 2)

--     colorNode :: Colour Double -> Maybe Int -> MazeBuilder (Maze Cardinal) (Diagram B)
--     colorNode colour ma = do
--         Config{..} <- ask
--         if not withColor
--             then return mempty
--             else do
--                 mx <- gets (fromIntegral . maxValue)
--                 return $ colorN ma mx colour
--       where
--         colorN :: Maybe Int -> Double -> Colour Double -> Diagram B
--         colorN Nothing _ _ = square 1 # fc grey lw 0
--         colorN (Just a) mx rCol = square 1 # fcA (col rCol ((mx - fromIntegral a) / mx)) lw none

--         col c v = toAlphaColour $ blend v c grey

--         maxValue m =
--             fromJust . _value $ maximumBy (compare `on` _value) (Map.elems m)

--     nodeToPoint (Node (NodeID (x, y)) _ _) = fromIntegral x ^& fromIntegral y
