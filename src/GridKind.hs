{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module GridKind where

import Data.Functor.Rep
import Data.Map
import Data.Proxy
import Draw (DrawMaze)
import MazeShape

class GridKind d where
    makeGrid :: Int -> Map NodeID (Node d (Maybe Int) Path)

data SomeGrid where
    SomeGrid ::
        ( Representable d
        , GridKind d
        , Opposite (Rep d)
        , Eq (Rep d)
        , Bounded (Rep d)
        , Enum (Rep d)
        , DrawMaze d
        , FromCardinalDir (Rep d)
        ) =>
        Proxy d ->
        SomeGrid

data CardinalDir = North | South | East | West
    deriving (Enum, Eq, Ord, Bounded, Show)

class FromCardinalDir a where
    fromCardinalDir :: CardinalDir -> a
