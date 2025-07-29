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
import Data.Functor.Rep (Representable (Rep, index, tabulate))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude hiding (Path, index, value)
import Draw
import GridKind
import MazeShape

instance ((Rep Cardinal ~ CardinalDir), Representable Cardinal) => GridKind Cardinal where
    makeGrid = newSquareGrid

newSquareGrid :: (Rep d ~ CardinalDir, Representable d) => Int -> Map.Map NodeID (Node d (Maybe a) Path)
newSquareGrid w =
    Map.fromList
        [(NodeID (x, y), mkNode (x, y)) | y <- [0 .. w - 1], x <- [0 .. w - 1]]
  where
    mkNode pos = Node (NodeID pos) Nothing (tabulate (mkPaths w pos))

mkPaths :: Int -> (Int, Int) -> CardinalDir -> MEdge Path
mkPaths w pos@(_, y) North = newConnection (y < w - 1) (NodeID pos) North
mkPaths _ pos@(_, y) South = newConnection (y > 0) (NodeID pos) South
mkPaths w pos@(x, _) East = newConnection (x < w - 1) (NodeID pos) East
mkPaths _ pos@(x, _) West = newConnection (x > 0) (NodeID pos) West

newConnection :: Bool -> NodeID -> CardinalDir -> MEdge Path
newConnection False _ _ = Nothing
newConnection True nid dir = Just $ Edge (directionNode nid dir) Closed

directionNode :: NodeID -> CardinalDir -> NodeID
directionNode (NodeID pos) North = NodeID $ pos .+. (0, 1)
directionNode (NodeID pos) South = NodeID $ pos .+. (0, -1)
directionNode (NodeID pos) East = NodeID $ pos .+. (1, 0)
directionNode (NodeID pos) West = NodeID $ pos .+. (-1, 0)

instance Opposite CardinalDir where
    opposite :: CardinalDir -> CardinalDir
    opposite North = South
    opposite South = North
    opposite East = West
    opposite West = East

instance FromCardinalDir CardinalDir where
    fromCardinalDir = id

data Cardinal a = Cardinal
    { _north :: a
    , _east :: a
    , _south :: a
    , _west :: a
    }
    deriving (Eq, Show)

makeLenses ''Cardinal

instance Functor Cardinal where
    fmap f (Cardinal n e s w) = Cardinal (f n) (f e) (f s) (f w)

-- >>> distribute (Just (Cardinal 1 2 3 4))
-- Cardinal {_north = Just 1, _east = Just 2, _south = Just 3, _west = Just 4}
instance Distributive Cardinal where
    distribute m = Cardinal (view north <$> m) (view east <$> m) (view south <$> m) (view west <$> m)

-- >>> index (Cardinal 1 2 3 4) South
-- 3
-- >>> tabulate (const 1) :: Cardinal Int
-- Cardinal {_north = 2, _east = 2, _south = 2, _west = 2}
instance Representable Cardinal where
    type Rep Cardinal = CardinalDir

    index :: Cardinal a -> (Rep Cardinal -> a)
    index c North = c ^. north
    index c East = c ^. east
    index c South = c ^. south
    index c West = c ^. west

    tabulate :: (Rep Cardinal -> a) -> Cardinal a
    tabulate c = Cardinal (c North) (c East) (c South) (c West)

instance Applicative Cardinal where
    pure :: a -> Cardinal a
    pure a = Cardinal a a a a

    (<*>) :: Cardinal (a -> b) -> Cardinal a -> Cardinal b
    (<*>) (Cardinal f1 f2 f3 f4) (Cardinal a b c d) = Cardinal (f1 a) (f2 b) (f3 c) (f4 d)

-- | for any representable type with an index that can be cast to a `Cardinal` direction, get the north and east components
northEastDirections :: (FromCardinalDir (Rep d), Representable d) => Node d a e -> [Maybe (Rep d, Edge e)]
northEastDirections n = [go North, go East]
  where
    go d = (fromCardinalDir d,) <$> index (n ^. directions) (fromCardinalDir d)

instance DrawMaze Cardinal where
    drawEdges (Cardinal n e s w) =
        [ (n, map p2 [(mHalfS, halfS), (halfS, halfS)])
        , (s, map p2 [(mHalfS, mHalfS), (halfS, mHalfS)])
        , (e, map p2 [(halfS, mHalfS), (halfS, halfS)])
        , (w, map p2 [(mHalfS, mHalfS), (mHalfS, halfS)])
        ]
      where
        side = 1.0
        halfS = side / 2
        mHalfS = -(side / 2)

    colorNode :: Colour Double -> Maybe Int -> MazeBuilder (Maze Cardinal) (Diagram B)
    colorNode colour ma = do
        Config{..} <- ask
        if not withColor
            then return mempty
            else do
                mx <- gets (fromIntegral . maxValue)
                return $ colorN ma mx colour
      where
        colorN :: Maybe Int -> Double -> Colour Double -> Diagram B
        colorN Nothing _ _ = square 1 # fc grey lw 0
        colorN (Just a) mx rCol = square 1 # fcA (col rCol ((mx - fromIntegral a) / mx)) lw none

        col c v = toAlphaColour $ blend v c grey

        maxValue m =
            fromJust . _value $ maximumBy (compare `on` _value) (Map.elems m)

    nodeToPoint (Node (NodeID (x, y)) _ _) = fromIntegral x ^& fromIntegral y
