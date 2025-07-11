{-# LANGUAGE FlexibleContexts #-}

-- | reports on the number of dead ends in a maze
module DeadEnds where

import Control.Lens (view)
import Control.Monad.RWS
import Data.Functor.Rep (Representable (..))
import qualified Data.Map as Map
import MazeShape (
    Maze,
    MazeBuilder,
    MazeNode,
    NodeID,
    nid,
    openConnections,
 )

getDeadEnds :: (Representable d, Bounded (Rep d), Enum (Rep d)) => MazeBuilder (Maze d) [NodeID]
getDeadEnds = do
    m <- get
    let nodes = Map.elems m
    return $ view nid <$> filter isDeadEnd nodes

-- | a node is dead end if it has only a single open connection
isDeadEnd :: (Representable d, Bounded (Rep d), Enum (Rep d)) => MazeNode d -> Bool
isDeadEnd node =
    length (openConnections node) == 1
