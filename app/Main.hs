{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedRecordDot       #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import qualified Algorithm.AldousBroder       as AldousBroder
import qualified Algorithm.BinaryTree         as BinaryTree
import qualified Algorithm.HuntKill           as HuntKill
import qualified Algorithm.RecursiveBacktrack as RecursiveBacktrack
import qualified Algorithm.Sidewinder         as Sidewinder
import qualified Algorithm.Wilson             as Wilson
import           App
import           DeadEnds                     (getDeadEnds)
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude             hiding (connect)
import           Draw                         (drawMaze)
import           Image                        (maskToBlankMaze)
import           Maze                         (Edge (..), Edges, Maze,
                                               NodeID (..), Path (..), connect,
                                               newMaze)
import           Options.Applicative
import           Param
import           Solve                        (findLongestRoute)

mycircle :: Diagram B
mycircle = circle 1

myLine :: Diagram B
myLine = (fromVertices $ map p2 [(0, 0), (100, 0), (100, 100), (0, 100)]) # lwO 20

myLine1 :: Diagram B
myLine1 = fromVertices $ map p2 [(0, 0), (100, 0)]

myLine2 :: Diagram B
myLine2 = fromVertices $ map p2 [(100, 0), (100, 100)]

drawEdges :: Edges Maze.Path -> Diagram B
drawEdges (n, s, e, w) =
  mconcat
    [ drawEdge n (map p2 [(0, 100), (100, 100)]),
      drawEdge s (map p2 [(0, 0), (100, 0)]),
      drawEdge e (map p2 [(100, 0), (100, 100)]),
      drawEdge w (map p2 [(0, 0), (0, 100)])
    ]
    # lwO 10

drawEdge :: (TrailLike t, Monoid t) => Maybe (Edge e) -> [Point (V t) (N t)] -> t
drawEdge Nothing p = fromVertices p
drawEdge (Just (Edge _ e)) p = case e of
  Open   -> mempty
  Closed -> fromVertices p

joined = (myLine1 <> myLine2) # lwO 15

nID = NodeID (0, 0)

exampleEdges = (Just (Edge nID Open), Just (Edge nID Closed), Just (Edge nID Closed), Just (Edge nID Closed))

exampleMaze = connect (NodeID (0, 0)) (NodeID (1, 0)) (newMaze 2)

main :: IO ()
main = do
  (diagram, _) <- m
  mainWith $ diagram
  where
    c = Config 1 30 False False False False Nothing
    m = runBuilder (BinaryTree.generateMaze >> Solve.findLongestRoute >> drawMaze [] []) c (newMaze c.mazeSize)

-- main :: IO ()
-- main = do
--   customExecParser p opts >>= run
--   where
--     opts =
--       info
--         (algorithm <**> helper)
--         (fullDesc <> progDesc "Create and solve random mazes!")
--     p = prefs showHelpOnEmpty

-- run :: Algorithm -> IO ()
-- run alg = do
--   case alg of
--     BinaryTree cfg -> do
--       runAlgorithm BinaryTree.generateMaze cfg
--     Sidewinder cfg -> do
--       runAlgorithm Sidewinder.generateMaze cfg
--     AldousBroder cfg -> do
--       runAlgorithm AldousBroder.generateMaze cfg
--     Wilson cfg -> do
--       runAlgorithm Wilson.generateMaze cfg
--     HuntKill cfg -> do
--       runAlgorithm HuntKill.generateMaze cfg
--     RecursiveBacktrack cfg -> do
--       runAlgorithm RecursiveBacktrack.generateMaze cfg
--   pure ()
--   where
--     runAlgorithm :: MazeBuilder Config Maze () -> Config -> IO ()
--     runAlgorithm generate c = do
--       let m = newMaze c.mazeSize
--       (solution, maze) <- runBuilder (maskToBlankMaze >> generate >> Solve.findLongestRoute) c m
--       (deadEnds, maze') <- runBuilder getDeadEnds c maze
--       (picture, _) <- runBuilder (drawMaze solution deadEnds) c maze'

-- display (InWindow "Maze" (100, 100) (startWindowPos c.lineLength c.mazeSize)) white picture
