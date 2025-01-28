-- | AldousBroder implements a random walk algorithm where we link nodes that are visited on the walk, finishing when all nodes have been visted.
module Algorithm.AldousBroder (generateMaze) where

import           App                  (MazeBuilder, randomNode)
import           Control.Monad.Random
import           Control.Monad.RWS
import qualified Data.Map             as Map
import           Data.Maybe           (catMaybes)
import qualified Data.Set             as Set
import           Maze                 (Edge (Edge), Maze, Node (Node, nid),
                                       NodeID (NodeID), Path (Closed, Open),
                                       connect)

generate ::  Set.Set NodeID -> NodeID -> MazeBuilder c Maze ()
generate visited nid = do
  maze <- get
  let visited' = Set.insert nid visited
  if length visited' == length maze
    then pure ()
    else case Map.lookup nid maze of
      (Just (Node _ _ n s e w)) -> do
        nNode <- uniform $ catMaybes [n, s, e, w]
        case nNode of
          (Edge nextID Open) -> generate visited' nextID
          (Edge nextID Closed) ->
            if Set.member nextID visited'
              then generate visited' nextID
              else do
                modify' $ connect nid nextID
                generate visited' nextID
      Nothing -> pure ()

generateMaze ::  MazeBuilder c Maze ()
generateMaze =
  randomNode >>= generate Set.empty . nid
