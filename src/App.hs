module App where

import           Control.Monad.Random
import           Control.Monad.RWS
import qualified Data.Map             as Map
import           Maze                 (Maze, MazeNode, NodeID)

data Config = Config
  { lineLength    :: Double,
    mazeSize      :: Int,
    solve         :: Bool,
    withColor     :: Bool,
    countDeadEnds :: Bool,
    debug         :: Bool,
    mask          :: Maybe FilePath
  }

type MazeBuilder config s = RWST Config () s IO

runBuilder :: MazeBuilder Config state a -> Config -> state -> IO (a, state)
runBuilder app c s = do
  (a, s', _) <- runRWST app c s
  return (a, s')

-- | This is a partial function to get nodes from IDs where we know that the node exists.
--  will panic if node is not in map.
getNode :: NodeID -> MazeBuilder c Maze MazeNode
getNode i = do
  m <- get
  case Map.lookup i m of
    Just n  -> return n
    Nothing -> error ("could not get node from ID: " ++ show i)

randomNode :: MazeBuilder c Maze MazeNode
randomNode = do
  m <- get
  uniform $ Map.elems m
