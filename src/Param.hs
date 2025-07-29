{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Param where

import qualified Algorithm.AldousBroder as AldousBroder
import qualified Algorithm.HuntKill as BinaryTree
import qualified Algorithm.HuntKill as HuntKill
import qualified Algorithm.RecursiveBacktrack as RecursiveBacktrack
import qualified Algorithm.Sidewinder as Sidewinder
import qualified Algorithm.Wilson as Wilson
import Data.Char (toLower)
import Data.Data (Proxy (Proxy))
import Data.Functor.Rep (Representable (..))
import Data.Map hiding (map)
import Draw (DrawMaze)
import GridKind (FromCardinalDir, SomeGrid (SomeGrid))
import MazeShape (Algorithm (..), Config (..), Maze, MazeBuilder, Node, NodeID, Opposite, Path, Shape (..))
import MazeShape.Sigma (Sigma, SigmaDir)
import MazeShape.Square (Cardinal)
import Options.Applicative

algorithmP :: Parser Algorithm
algorithmP =
    hsubparser
        ( command "binary" (info (pure BinaryTree) (progDesc "pick from two options for each space"))
            <> command "sidewinder" (info (pure Sidewinder) (progDesc "similar to binary but pick from nodes of carved spaces"))
            <> command "aldous" (info (pure AldousBroder) (progDesc "randomly walk until all nodes are visited"))
            <> command "wilson" (info (pure Wilson) (progDesc "randomly walk until nodes with loop checking"))
            <> command "hunt" (info (pure HuntKill) (progDesc "random walk but avoiding visiting the same nodes"))
            <> command
                "recursive"
                (info (pure RecursiveBacktrack) (progDesc "random walk but avoiding visiting the same nodes - with backtracking"))
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
        <*> algorithmP
        <*> option
            parseShape
            ( long "shape"
                <> metavar "square|hex"
                <> help "Grid shape (square or hex)"
            )

-- <*> optional
--     ( strOption
--         ( long "mask"
--             <> short 'm'
--             <> help
--                 "filepath to mask png file. Maze size will be ignored if this option is set. Does not work with some maze building algorthims."
--         )
--     )

algorithmFun ::
    (Representable d, Bounded (Rep d), Enum (Rep d), Eq (Rep d), Opposite (Rep d), FromCardinalDir (Rep d)) =>
    Algorithm ->
    MazeBuilder (Maze d) ()
algorithmFun BinaryTree = BinaryTree.generateMaze
algorithmFun Sidewinder = Sidewinder.generateMaze
algorithmFun AldousBroder = AldousBroder.generateMaze
algorithmFun Wilson = Wilson.generateMaze
algorithmFun HuntKill = HuntKill.generateMaze
algorithmFun RecursiveBacktrack = RecursiveBacktrack.generateMaze

parseShape :: ReadM Shape
parseShape = eitherReader $ \arg ->
    case map toLower arg of
        "square" -> Right Square
        "hex" -> Right Hexagon
        "hexagon" -> Right Hexagon
        _ -> Left "Shape must be 'square' or 'hex'"

shapeToGrid :: Shape -> SomeGrid
shapeToGrid Square = SomeGrid (Proxy :: Proxy Cardinal)
shapeToGrid Hexagon = SomeGrid (Proxy :: Proxy Sigma)
