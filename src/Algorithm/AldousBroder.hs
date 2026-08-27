{-# LANGUAGE FlexibleContexts #-}

-- | AldousBroder implements a random walk algorithm where we link nodes that are visited on the walk, finishing when all nodes have been visted.
module Algorithm.AldousBroder (generateMaze) where

import Control.Monad.RWS (MonadState (get), modify')
import Control.Monad.Random (uniform)
import qualified Data.Set as Set
import MazeShape (
    EdgeState (Closed, Open),
    GridShape,
    Maze,
    MazeBuilder,
    allCoords,
    connectEdge,
    getEdges,
    randomNode,
 )

generate ::
    (GridShape coord, Ord coord) =>
    Set.Set coord ->
    coord ->
    MazeBuilder (Maze coord a) ()
generate visited coord = do
    maze <- get
    let visited' = Set.insert coord visited
    if length visited' == length (allCoords maze)
        then pure ()
        else do
            (next, state) <- uniform $ getEdges coord maze
            case state of
                Open -> generate visited' next
                Closed ->
                    if Set.member next visited'
                        then generate visited' next
                        else do
                            modify' $ connectEdge coord next
                            generate visited' next

generateMaze ::
    (GridShape coord, Ord coord) =>
    MazeBuilder (Maze coord a) ()
generateMaze =
    randomNode >>= generate Set.empty
