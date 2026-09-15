{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module GridKind where

import Data.Proxy
import MazeShape

class GridKind d where
    makeGrid :: Int -> Maze d ()

data SomeGrid where
    SomeGrid ::
        ( GridKind d
        , GridShape d
        , Ord d
        , Show d
        , NorthEastDirection (Direction d)
        ) =>
        Proxy d ->
        SomeGrid
