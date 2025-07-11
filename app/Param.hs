module Param where

import MazeShape (Config (..))
import Options.Applicative

data Algorithm
    = BinaryTree Config
    | Sidewinder Config
    | AldousBroder Config
    | Wilson Config
    | HuntKill Config
    | RecursiveBacktrack Config

algorithm :: Parser Algorithm
algorithm =
    hsubparser
        ( command "binary" (info (BinaryTree <$> config) (progDesc "pick from two options for each space"))
            <> command "sidewinder" (info (Sidewinder <$> config) (progDesc "similar to binary but pick from nodes of carved spaces"))
            <> command "aldous" (info (AldousBroder <$> config) (progDesc "randomly walk until all nodes are visited"))
            <> command "wilson" (info (Wilson <$> config) (progDesc "randomly walk until nodes with loop checking"))
            <> command "hunt" (info (HuntKill <$> config) (progDesc "random walk but avoiding visiting the same nodes"))
            <> command
                "recursive"
                (info (RecursiveBacktrack <$> config) (progDesc "random walk but avoiding visiting the same nodes - with backtracking"))
        )

config :: Parser Config
config =
    Config
        <$> option auto (long "diagram_size" <> short 'l' <> short 'w' <> value 30 <> help "length or width of the diagram to draw")
        <*> option auto (long "maze_size" <> short 's' <> value 10 <> help "number of squares in maze")
        <*> switch (long "solve" <> help "print a solution")
        <*> switch (long "color" <> help "color the solution of the maze to see bias")
        <*> switch (long "deadends" <> short 'd' <> help "count the number of dead ends in maze")
        <*> switch (long "debug" <> help "print the Djkstra distances of the maze")
        <*> strOption (long "file_name" <> short 'n' <> value "maze.svg" <> help "the file name to save the maze to")

-- <*> optional
--     ( strOption
--         ( long "mask"
--             <> short 'm'
--             <> help
--                 "filepath to mask png file. Maze size will be ignored if this option is set. Does not work with some maze building algorthims."
--         )
--     )

startWindowPos :: Float -> Int -> (Int, Int)
startWindowPos len size =
    ( (size * round len) `div` 4
    , (size * round len) `div` 4
    )
