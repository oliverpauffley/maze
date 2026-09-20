{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Mazes with hexagonal shaped nodes.
module MazeShape.Sigma where

import Control.Monad (filterM)
import Control.Monad.State (MonadState (get), State, evalState, gets, modify')
import qualified Data.Map as Map
import Data.Map.Strict (Map, fromList)
import Data.Set (Set, insert, notMember)
import Diagrams.Prelude hiding (Direction, center)
import GridKind (GridKind (makeGrid))
import MazeShape (
    EdgeState (Closed),
    GridShape (..),
    Maze (Maze),
    NodeShape (NodeShape),
    NorthEastDirection (..),
    edgeKey,
    nodeNeighbours,
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

{- | Hexagonal grids using the "offset" coordinate system
see https://www.redblobgames.com/grids/hexagons/#coordinates
-}
newtype Sigma = Sigma (Int, Int)
    deriving (Show, Eq, Ord)

instance GridShape Sigma where
    data Direction Sigma = North | South | NorthEast | NorthWest | SouthEast | SouthWest
        deriving (Show, Enum, Bounded, Eq, Ord)

    neighbor :: Sigma -> Direction Sigma -> Maybe Sigma
    neighbor (Sigma (q, r)) North = Just $ Sigma (q, r - 1)
    neighbor (Sigma (q, r)) South = Just $ Sigma (q, r + 1)
    neighbor (Sigma (q, r)) NorthEast | even q = Just $ Sigma (q + 1, r - 1)
    neighbor (Sigma (q, r)) NorthEast | odd q = Just $ Sigma (q + 1, r)
    neighbor (Sigma (q, r)) NorthWest | even q = Just $ Sigma (q - 1, r - 1)
    neighbor (Sigma (q, r)) NorthWest | odd q = Just $ Sigma (q - 1, r)
    neighbor (Sigma (q, r)) SouthEast | even q = Just $ Sigma (q + 1, r)
    neighbor (Sigma (q, r)) SouthEast | odd q = Just $ Sigma (q + 1, r + 1)
    neighbor (Sigma (q, r)) SouthWest | even q = Just $ Sigma (q - 1, r)
    neighbor (Sigma (q, r)) SouthWest | odd q = Just $ Sigma (q - 1, r + 1)

    toShape :: Sigma -> NodeShape (Direction Sigma)
    toShape (Sigma pos) = NodeShape point (sigmaEdges point)
      where
        point = posToPoint pos

posToPoint :: (Int, Int) -> Point V2 Double
posToPoint (p, r')
    | odd p =
        (fromIntegral p * (xFarEast + xNearEast)) ^& (ySouth + fromIntegral r * yNorth * 2)
    | otherwise = (fromIntegral p * (xFarEast + xNearEast)) ^& (fromIntegral r * (yNorth * 2))
  where
    r = -r'

sigmaEdges :: Point V2 Double -> Map (Direction Sigma) (Located (Trail V2 Double))
sigmaEdges c =
    fromList $
        zip
            [North, NorthEast, SouthEast, South, SouthWest, NorthWest]
            ( map
                (fromVertices . (map (\p -> c ^+^ p)))
                [ [(xNearWest ^& yNorth), (xNearEast ^& yNorth)]
                , [(xNearEast ^& yNorth), (xFarEast ^& center)]
                , [(xFarEast ^& center), (xNearEast ^& ySouth)]
                , [(xNearEast ^& ySouth), (xNearWest ^& ySouth)]
                , [(xNearWest ^& ySouth), (xFarWest ^& center)]
                , [(xFarWest ^& center), (xNearWest ^& yNorth)]
                ]
            )

newSigmaGrid :: Int -> Maze Sigma ()
newSigmaGrid s =
    Maze
        ( foldl'
            (\m n -> Map.insert n () m)
            mempty
            ns
        )
        ( foldl'
            (\m n -> Map.insert n Closed m)
            mempty
            es
        )
  where
    (ns, es) = evalState (buildGrid s [Sigma (0, 0)]) mempty

type BuildState a = State (Set Sigma, Set (Sigma, Sigma)) a

notSeen :: Sigma -> BuildState Bool
notSeen n = gets (\(s, _) -> notMember n s)

-- | when we are building a grid add the node by recording the edges we need to connect and that we have seen the node on our walk.
addNode :: Sigma -> [Sigma] -> BuildState ()
addNode node edges =
    modify'
        ( \(ns, es) ->
            (insert node ns, foldl' insertEdge es edges)
        )
  where
    insertEdge es e = (\key -> insert key es) $ edgeKey node e

-- | build a sigma grid by walking through each node in the grid and tracking connections and seen nodes as we go.
buildGrid ::
    Int -> [Sigma] -> BuildState (Set Sigma, Set (Sigma, Sigma))
buildGrid _ [] = get
buildGrid size (x : xs) = do
    let ns = filter (inbounds size) $ neighbors x
    next <- filterM notSeen ns
    addNode x next
    buildGrid size (xs ++ next)

-- | returns true if the given coordinate is within the shape size.
inbounds :: Int -> Sigma -> Bool
inbounds size (Sigma (q, r)) = q >= 0 && r >= 0 && q < size && r < size

instance GridKind Sigma where
    makeGrid = newSigmaGrid

instance NorthEastDirection (Direction Sigma) where
    northDir :: Direction Sigma
    northDir = North
    eastDir :: Direction Sigma
    eastDir = SouthEast
