{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Draw where

import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (Path, value)
import MazeShape (
    Config (..),
    Edge (Edge),
    Maze,
    MazeBuilder,
    MazeNode,
    Node (Node),
    NodeID (NodeID),
    Path (Closed, Open),
    value,
 )

type Solution = [NodeID]
type DeadEnds = [NodeID]

class DrawMaze a where
    drawMaze ::
        Solution ->
        DeadEnds ->
        MazeBuilder (Maze a) (Diagram B)
