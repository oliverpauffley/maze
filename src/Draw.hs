{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Draw where

import Control.Monad.Random (MonadIO, Random, randomIO)
import Control.Monad.Reader (MonadReader (ask), asks)
import Control.Monad.State (get)
import Data.Colour.SRGB.Linear (rgb)
import qualified Data.Map as Map
import Debug.Trace (traceShow)
import Diagrams.Backend.SVG
import Diagrams.Prelude hiding (Path, value)
import MazeShape (
    Config (..),
    Edge (Edge),
    MEdge,
    Maze,
    MazeBuilder,
    Node,
    NodeID,
    Path (Closed, Open),
    directions,
    getNode,
    value,
 )

type Solution = [NodeID]
type DeadEnds = [NodeID]

class DrawMaze a where
    drawMaze :: Solution -> DeadEnds -> MazeBuilder (Maze a) (Diagram B)
    drawMaze solution deadEnds = do
        maze <- get
        rColour <- getRandomColor
        mazePicture <- mapM (drawNode rColour) (Map.elems maze)
        solutionPath <- drawSolution (getNodes solution maze)
        deadEndCount <- drawDeadEnds deadEnds
        return $ vsep 1 [solutionPath <> position mazePicture, deadEndCount] # frame 0.3

    nodeToPoint :: Node a v p -> Point V2 Double
    drawEdges :: a (MEdge Path) -> [(MEdge Path, [Point V2 Double])]
    colorNode :: Colour Double -> Maybe Int -> MazeBuilder (Maze a) (Diagram B)

drawNode ::
    (DrawMaze a) => Colour Double -> Node a (Maybe Int) Path -> MazeBuilder (Maze a) (Point V2 Double, Diagram B)
drawNode col node = do
    debugLabels <- labels (node ^. value)
    colors <- colorNode col (node ^. value)
    let es = edges (node ^. directions)
        nodePos = nodeToPoint node
        ds = [es, debugLabels, colors]
        posDs = zip (repeat (p2 (0.0, 0.0))) ds
    return (nodePos, position posDs)

getNodes :: [NodeID] -> Maze d -> [Node d (Maybe Int) Path]
getNodes ns maze = map (getNode maze) ns

drawSolution :: (DrawMaze d) => [Node d (Maybe Int) Path] -> MazeBuilder m (Diagram B)
drawSolution solution = do
    Config{..} <- ask
    if solve
        then
            return $
                strokePath (fromVertices $ map nodeToPoint solution)
                    # lc red
                    # lw 2
                    # lineCap LineCapRound
                    # lineJoin LineJoinRound
        else return mempty

drawDeadEnds :: [NodeID] -> MazeBuilder s (Diagram B)
drawDeadEnds ns = do
    Config{..} <- ask
    if countDeadEnds
        then return (text ("Dead ends: " <> show (length ns)))
        else return mempty

getRandomColor :: (Random a, MonadIO m, Fractional a) => m (Colour a)
getRandomColor = do
    r <- randomIO
    g <- randomIO
    b <- randomIO
    return $ rgb r g b

labels :: (Show a) => Maybe a -> MazeBuilder m (Diagram B)
labels Nothing = return mempty
labels (Just a) = do
    debug <- asks debug
    if not debug
        then return mempty
        else
            return $ scale 0.5 (text (show a))

-- | generically draw all edges
edges :: (DrawMaze d) => d (MEdge Path) -> Diagram B
edges = mconcat . map (uncurry drawEdge) . drawEdges # lwO 10 # lineCap LineCapRound # lineJoin LineJoinRound

drawEdge :: (TrailLike t, Monoid t) => Maybe (Edge Path) -> [Point (V t) (N t)] -> t
drawEdge Nothing p = fromVertices p
drawEdge (Just (Edge _ e)) p = case e of
    Open -> mempty
    Closed -> fromVertices p
