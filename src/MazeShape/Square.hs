{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module MazeShape.Square where

import Data.Foldable (maximumBy)

import Control.Monad.Random (MonadIO, Random, randomIO)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, gets)
import Data.Colour.SRGB.Linear (rgb)
import Data.Distributive (Distributive (distribute))
import Data.Function (on)
import Data.Functor.Rep (Representable (Rep, index, tabulate))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude hiding (Path, index, value)
import Draw
import MazeShape

type Width = Int

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

-- | Pointwise addition
(.+.) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)

data CardinalDir = North | South | East | West
    deriving (Enum, Eq, Ord, Bounded)

instance Opposite CardinalDir where
    opposite :: CardinalDir -> CardinalDir
    opposite North = South
    opposite South = North
    opposite East = West
    opposite West = East

class FromCardinalDir a where
    fromCardinalDir :: CardinalDir -> a

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
    index (Cardinal n _ _ _) North = n
    index (Cardinal _ e _ _) East = e
    index (Cardinal _ _ s _) South = s
    index (Cardinal _ _ _ w) West = w

    tabulate :: (Rep Cardinal -> a) -> Cardinal a
    tabulate b = Cardinal (b North) (b East) (b South) (b West)

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
    drawMaze solution deadEnds = do
        maze <- get
        rColour <- getRandomColor
        mazePicture <- mapM (drawNode rColour) (Map.elems maze)
        solutionPath <- drawSolution solution
        deadEndCount <- drawDeadEnds deadEnds
        return $ vsep 1 [position mazePicture <> solutionPath, deadEndCount]

drawNode :: Colour Double -> Node Cardinal (Maybe Int) Path -> MazeBuilder (Maze Cardinal) (Point V2 Double, Diagram B)
drawNode col (Node (NodeID (x, y)) val dirs) = do
    debugLabels <- labels val
    colors <- colorNode col val
    let (n, s, e, w) = (view north dirs, view south dirs, view east dirs, view west dirs)
    edges <- drawEdges (n, s, e, w)
    let pos = [p2 (0, 0), p2 (0.5, 0.3), p2 (0.5, 0.5)]
        ds = [edges, debugLabels, colors]
        posDs = zip pos ds
    return (fromIntegral x ^& fromIntegral y, position posDs)

labels :: (Show a) => Maybe a -> MazeBuilder m (Diagram B)
labels Nothing = return mempty
labels (Just a) = do
    debug <- asks debug
    if not debug
        then return mempty
        else
            return $ scale 0.5 (text (show a))

drawSolution :: [NodeID] -> MazeBuilder m (Diagram B)
drawSolution solution = do
    Config{..} <- ask
    if solve
        then return $ strokePath (fromVertices $ map toPoint solution) # lc red # lw 2
        else return mempty
  where
    toPoint (NodeID (x, y)) = p2 (fromIntegral x + 0.5, fromIntegral y + 0.5)

drawEdges :: (MEdge Path, MEdge Path, MEdge Path, MEdge Path) -> MazeBuilder m (Diagram B)
drawEdges (n, s, e, w) = do
    return $
        mconcat
            [ drawEdge n (map p2 [(0, 1), (1, 1)])
            , drawEdge s (map p2 [(0, 0), (1, 0)])
            , drawEdge e (map p2 [(1, 0), (1, 1)])
            , drawEdge w (map p2 [(0, 0), (0, 1)])
            ]
            # lwO 10

drawEdge :: (TrailLike t, Monoid t) => Maybe (Edge Path) -> [Point (V t) (N t)] -> t
drawEdge Nothing p = fromVertices p
drawEdge (Just (Edge _ e)) p = case e of
    Open -> mempty
    Closed -> fromVertices p

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
    colorN Nothing _ _ = square 1.01 # fc black lw 0
    colorN (Just a) mx rCol = square 1.01 # fcA (col rCol ((mx - fromIntegral a) / mx)) lw none

    col c v = toAlphaColour $ blend v c black

    maxValue m =
        fromJust . _value $ maximumBy (compare `on` _value) (Map.elems m)

getRandomColor :: (Random a, MonadIO m, Fractional a) => m (Colour a)
getRandomColor = do
    r <- randomIO
    g <- randomIO
    b <- randomIO
    return $ rgb r g b

drawDeadEnds :: [NodeID] -> MazeBuilder s (Diagram B)
drawDeadEnds ns = do
    Config{..} <- ask
    if countDeadEnds
        then return (text ("Dead ends: " <> show (length ns)))
        else return mempty
