{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}

module Draw where

import           App                     (Config (..), MazeBuilder)
import           Control.Monad.Random    (randomIO)
import           Control.Monad.RWS       (MonadReader (ask), MonadState (get),
                                          asks)
import           Data.Colour.SRGB.Linear (rgb)
import           Data.Foldable           (maximumBy)
import           Data.Function           (on)
import qualified Data.Map                as Map
import           Data.Maybe              (fromJust)
import           Diagrams.Backend.SVG
import           Diagrams.Prelude        hiding (value)
import           Maze                    (Edge (Edge), Edges, Maze, Node (Node),
                                          NodeID (NodeID), Path (Closed, Open),
                                          mazeToList, value)

drawMaze :: [NodeID] -> [NodeID] -> MazeBuilder Config Maze (Diagram B)
drawMaze solution deadEnds = do
  Config {..} <- ask
  maze <- get
  rColour <- getRandomColor
  mazePicture <- mapM (drawNode rColour) (mazeToList maze)
  solutionPath <- drawSolution solution
  -- deadEndCount <- drawDeadEnds deadEnds
  return $ position mazePicture <> solutionPath

drawNode :: Colour Double -> Node (Maybe Int) Maze.Path -> MazeBuilder Config Maze (Point V2 Double, Diagram B)
drawNode col (Node (NodeID (x, y)) val n s e w) = do
  debugLabels <- labels val
  colors <- colorNode col val
  edges <- drawEdges (n, s, e, w)
  let pos = [p2 (0, 0), p2 (0.5, 0.3), p2 (0.5, 0.5)]
      ds = [edges, debugLabels, colors]
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

colorNode :: Colour Double -> Maybe Int -> MazeBuilder Config Maze (Diagram B)
colorNode colour ma = do
  Config {..} <- ask
  if not withColor
    then return mempty
    else do
      mx <- fromIntegral . maxValue <$> get
      return $ colorN ma mx colour
  where
    colorN :: Maybe Int -> Double -> Colour Double -> Diagram B
    colorN Nothing _ _ = square 1.01 # fc black lw 0
    colorN (Just a) mx rCol = square 1.01 # fcA (col rCol ((mx - fromIntegral a) / mx)) lw none

    col colour v = toAlphaColour $ blend v colour black

    maxValue m =
      fromJust . value $ maximumBy (compare `on` value) (Map.elems m)

getRandomColor :: MazeBuilder c m (Colour Double)
getRandomColor = do
  r <- randomIO
  g <- randomIO
  b <- randomIO
  return $ rgb r g b

drawDeadEnds :: [NodeID] -> MazeBuilder Config s (Diagram B)
drawDeadEnds ns = do
  Config {..} <- ask
  if countDeadEnds
    then return (text ("Dead ends: " <> show (length ns)))
    else return mempty
