{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}

module Draw where

import           App                  (Config (..), MazeBuilder)
import           Control.Monad.RWS    (MonadReader (ask), MonadState (get),
                                       asks)
import qualified Data.Map             as Map
import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           Maze                 (Edge (Edge), Edges, Maze, Node (Node),
                                       NodeID (NodeID), Path (Closed, Open),
                                       mazeToList, nid, value)

drawMaze :: [NodeID] -> [NodeID] -> MazeBuilder Config Maze (Diagram B)
drawMaze solution deadEnds = do
  Config {..} <- ask
  maze <- get
  mazePicture <- mapM drawNode (mazeToList maze)
  solutionPath <- drawSolution solution
  -- deadEndCount <- drawDeadEnds deadEnds
  return $ position mazePicture <> solutionPath

maxSolutionInMaze :: [NodeID] -> Maze -> Maybe Int
maxSolutionInMaze xs m = do
  square <- Map.lookup (head xs) m
  Maze.value square

drawNode (Node (NodeID (x, y)) val n s e w) = do
  debugLabels <- labels val
  colors <- colorNode val
  edges <- drawEdges (n, s, e, w)
  let pos = [p2 (0, 0), p2 (0.5, 0.3), p2 (0.5, 0.5)]
      ds = [edges,debugLabels, colors]
      posDs = zip pos ds
  return (fromIntegral x ^& fromIntegral y, position posDs)

labels :: (Show a) => Maybe a -> MazeBuilder Config m (Diagram B)
labels Nothing = return mempty
labels (Just a) = do
  debug <- asks debug
  if not debug
    then return mempty
    else
      return $ scale 0.5 (text (show a))

drawSolution :: [NodeID] -> MazeBuilder Config m (Diagram B)
drawSolution solution = do
  Config {..} <- ask
  if solve
    then return $ strokePath (fromVertices $ map toPoint solution) # lc red # lw 2
    else return mempty
  where
    toPoint (NodeID (x, y)) = p2 (fromIntegral x + 0.5, fromIntegral y + 0.5)

drawEdges :: Edges Maze.Path -> MazeBuilder Config m (Diagram B)
drawEdges (n, s, e, w) = do
  return $
    mconcat
      [ drawEdge n (map p2 [(0, 1), (1, 1)]),
        drawEdge s (map p2 [(0, 0), (1, 0)]),
        drawEdge e (map p2 [(1, 0), (1, 1)]),
        drawEdge w (map p2 [(0, 0), (0, 1)])
      ]
      # lwO 10

drawEdge :: (TrailLike t, Monoid t) => Maybe (Edge e) -> [Point (V t) (N t)] -> t
drawEdge Nothing p = fromVertices p
drawEdge (Just (Edge _ e)) p = case e of
  Open   -> mempty
  Closed -> fromVertices p

colorNode :: Maybe Int -> MazeBuilder Config m (Diagram B)
colorNode ma = do
  Config {..} <- ask
  if not withColor
    then return mempty
    else return $ colorN ma mazeSize
  where
    colorN :: Maybe Int -> Int -> Diagram B
    colorN Nothing _ = square 1 # fc black lw 0
    colorN (Just a) mS = square 1 # fcA (colorMix (fromIntegral mS) (fromIntegral a)) lw 0

    colorMix m val = yellow `withOpacity` colorScale m val
    colorScale m val = (val - m) / m

drawDeadEnds :: [NodeID] -> MazeBuilder Config s (Diagram B)
drawDeadEnds ns = do
  Config {..} <- ask
  if countDeadEnds
    then return (text ("Dead ends: " <> show (length ns)))
    else return mempty
