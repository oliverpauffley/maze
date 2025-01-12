{-# LANGUAGE RecordWildCards #-}

module Draw where

import           App               (Config (..), MazeBuilder)
import           Control.Monad     (guard)
import           Control.Monad.RWS (MonadReader (ask), MonadState (get),
                                    MonadWriter (listen), asks)
import qualified Data.Map          as Map
import qualified Graphics.Gloss    as Gloss
import           Maze              (Edge (Edge), Edges, Maze, Node (Node),
                                    NodeID (NodeID), Path (Closed, Open),
                                    mazeToList, value)

drawMaze :: [NodeID] -> MazeBuilder Config [NodeID] Maze Gloss.Picture
drawMaze solution = do
  Config {..} <- ask
  maze <- get
  let (Just m) = maxSolutionInMaze solution maze
  mazePicture <- mapM (drawNode m) (mazeToList maze)
  solutionPicture <- drawSolution solution
  return $ Gloss.pictures (solutionPicture : mazePicture)

maxSolutionInMaze :: [NodeID] -> Maze -> Maybe Int
maxSolutionInMaze xs m = do
  square <- Map.lookup (last xs) m
  value square

drawNode :: Int -> Node (Maybe Int) Maze.Path -> MazeBuilder Config [NodeID] Maze Gloss.Picture
drawNode maxSol (Node (NodeID (x, y)) val n s e w) = do
  Config {..} <- ask
  debugLabels <- labels val
  colors <- colorNode maxSol val
  edges <- drawEdges (n, s, e, w)
  return $
    Gloss.Pictures $
      map (translation lineLength x y) (debugLabels : colors : edges)
  where
    translation ll a b = Gloss.translate (fromIntegral a * ll) (fromIntegral b * ll)

labels :: (Show a) => Maybe a -> MazeBuilder Config [NodeID] m Gloss.Picture
labels Nothing = return mempty
labels (Just a) = do
  Config {..} <- ask
  if not debug
    then return mempty
    else
      return $
        Gloss.translate (lineLength / 2) (lineLength / 2) $
          Gloss.scale 0.1 0.1 $
            Gloss.text $
              show a

drawSolution :: [NodeID] -> MazeBuilder Config [NodeID] m Gloss.Picture
drawSolution solution = do
  Config {..} <- ask
  if solve
    then return $ Gloss.Color Gloss.red $ Gloss.line $ map (pos lineLength . toPath) solution
    else return mempty
  where
    pos ll (x, y) = (x * ll + ll / 2, y * ll + ll / 2)
    toPath (NodeID (x, y)) = (fromIntegral x, fromIntegral y)

drawEdges :: Edges Maze.Path -> MazeBuilder Config [NodeID] m [Gloss.Picture]
drawEdges (n, s, e, w) = do
  ll <- asks lineLength
  return
    [ drawEdge n [(0, ll), (ll, ll)],
      drawEdge s [(0, 0), (ll, 0)],
      drawEdge e [(ll, 0), (ll, ll)],
      drawEdge w [(0, 0), (0, ll)]
    ]

colorNode :: Int -> Maybe Int -> MazeBuilder Config [NodeID] m Gloss.Picture
colorNode solMax ma = do
  Config {..} <- ask
  if not withColor
    then return mempty
    else return $ trans lineLength $ colorN lineLength ma
  where
    trans ll = Gloss.translate (ll / 2) (ll / 2)

    colorN ll Nothing  = Gloss.color Gloss.black $ rec ll
    colorN ll (Just a) = colorScale solMax a $ rec ll

    rec ll = Gloss.rectangleSolid ll ll

    colorScale m val
      | val < m = Gloss.color Gloss.white
      | otherwise = Gloss.color $ Gloss.mixColors (fromIntegral m) (fromIntegral val) greenSmoke Gloss.black

greenSmoke :: Gloss.Color
greenSmoke = Gloss.makeColorI 113 123 112 1

drawEdge :: Maybe (Maze.Edge Maze.Path) -> Gloss.Path -> Gloss.Picture
drawEdge Nothing p = Gloss.Line p
drawEdge (Just (Edge _ e)) path = case e of
  Open   -> Gloss.Blank
  Closed -> Gloss.Line path

-- TODO look into simulate from Gloss to show generation steps
