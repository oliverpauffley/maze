{-# LANGUAGE FlexibleContexts #-}

-- | AldousBroder implements a random walk algorithm where we link nodes that are visited on the walk, finishing when all nodes have been visted.
module Algorithm.AldousBroder (generateMaze) where

import Control.Lens (view)
import Control.Monad.RWS (MonadState (get), modify')
import Control.Monad.Random (uniform)
import Data.Functor.Rep (Representable (Rep))
import qualified Data.Set as Set
import MazeShape (
    Edge (Edge),
    Maze,
    MazeBuilder,
    NodeID (NodeID),
    Opposite,
    Path (Closed, Open),
    connectNodes,
    connections,
    getNode,
    nid,
    randomNode,
    _nid,
 )

generate ::
    (Representable d, Opposite (Rep d), Eq (Rep d), Bounded (Rep d), Enum (Rep d)) =>
    Set.Set NodeID ->
    NodeID ->
    MazeBuilder (Maze d) ()
generate visited nid = do
    maze <- get
    let visited' = Set.insert nid visited
    if length visited' == length maze
        then pure ()
        else do
            let n = getNode maze nid
            (nNode, dir) <- uniform $ connections n
            case nNode of
                (Edge nextID Open) -> generate visited' nextID
                (Edge nextID Closed) ->
                    if Set.member nextID visited'
                        then generate visited' nextID
                        else do
                            modify' $ connectNodes nid dir
                            generate visited' nextID

generateMaze ::
    (Representable d, Opposite (Rep d), Eq (Rep d), Bounded (Rep d), Enum (Rep d)) => MazeBuilder (Maze d) ()
generateMaze =
    randomNode >>= generate Set.empty . view nid
