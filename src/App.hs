module App where

import           Control.Monad.RWS

data Config = Config
  { lineLength :: Float,
    mazeSize   :: Int,
    solve      :: Bool,
    withColor  :: Bool
  }

type MazeBuilder config w s = RWST Config w s IO

runBuilder :: MazeBuilder Config w state a -> Config -> state -> IO (a, state, w)
runBuilder = runRWST
