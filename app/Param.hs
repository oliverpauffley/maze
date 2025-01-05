{-# LANGUAGE OverloadedRecordDot #-}

module Param where

import qualified BinaryTree
import           Control.Monad.Trans.State.Strict
import qualified Data.Map                         as Map
import           Draw                             (drawMaze)
import           Graphics.Gloss
import           Maze                             (newMaze)
import           Options.Applicative
import qualified Sidewinder

data Config = Config
  { lineLength :: Float,
    mazeSize   :: Int
  }

data Algorithm = BinaryTree Config | Sidewinder Config

algorithm :: Parser Algorithm
algorithm =
  hsubparser
    ( command "binary" (info (BinaryTree <$> config) (progDesc "pick from two options for each space"))
        <> command "sidewinder" (info (Sidewinder <$> config) (progDesc "similar to binary but pick from cells of carved spaces"))
    )

config :: Parser Config
config =
  Config
    <$> option auto (long "line_length" <> short 'l' <> value 30 <> help "line length for displaying maze")
    <*> option auto (long "maze_size" <> short 's' <> value 10 <> help "number of squares in maze")

startWindowPos :: Float -> Int -> (Int, Int)
startWindowPos len size =
  ( (size * round len) `div` 4,
    (size * round len) `div` 4
  )

run :: Algorithm -> IO ()
run alg =
  case alg of
    BinaryTree cfg -> do
      let m = newMaze cfg.mazeSize
      maze <- execStateT (traverse BinaryTree.generate (Map.keys m)) m
      display (InWindow "Maze" (100, 100) (startWindowPos cfg.lineLength cfg.mazeSize)) white (drawMaze cfg.lineLength maze)
    Sidewinder cfg -> do
      let m = newMaze cfg.mazeSize
      maze <- execStateT (traverse Sidewinder.generate (Map.keys m)) m
      display (InWindow "Maze" (100, 100) (startWindowPos cfg.lineLength cfg.mazeSize)) white (drawMaze cfg.lineLength maze)
