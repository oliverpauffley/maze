module App where

import           Control.Monad.Random
import           Control.Monad.RWS
import qualified Data.Map             as Map
import           Maze                 (Maze, MazeNode, NodeID)

data Config = Config
  { lineLength    :: Float,
    mazeSize      :: Int,
    solve         :: Bool,
    withColor     :: Bool,
    countDeadEnds :: Bool,
    debug         :: Bool,
    mask          :: Maybe FilePath
  }

type MazeBuilder config w s = RWST Config w s IO

runBuilder :: MazeBuilder Config w state a -> Config -> state -> IO (a, state, w)
runBuilder = runRWST

-- | This is a partial function to get nodes from IDs where we know that the node exists.
--  will panic if node is not in map.
getNode :: (Monoid w) => NodeID -> MazeBuilder c w Maze MazeNode
getNode i = do
  m <- get
  case Map.lookup i m of
    Just n  -> return n
    Nothing -> error ("could not get node from ID: " ++ show i)

randomNode :: (Monoid w) => MazeBuilder c w Maze MazeNode
randomNode = do
  m <- get
  uniform $ Map.elems m
