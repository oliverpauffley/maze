
module Param where

import           Options.Applicative

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
