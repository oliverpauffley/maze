{-# LANGUAGE RecordWildCards #-}

module Draw where

import           App                         (Config (..), MazeBuilder)
import           Control.Monad.RWS           (MonadReader (ask),
                                              MonadState (get),
                                              MonadWriter (listen))
import           Graphics.Gloss              (mixColors)
import qualified Graphics.Gloss              (Path,
                                              Picture (Blank, Line, Pictures),
                                              scale, text)
import qualified Graphics.Gloss              as Gloss
import           Graphics.Gloss.Data.Picture (translate)
import           Maze                        (Edge (Edge), Edges, Maze,
                                              Node (Node), NodeID (NodeID),
                                              Path (Closed, Open), mazeToList)
import qualified Solve

drawMaze :: MazeBuilder Config [NodeID] Maze Gloss.Picture
drawMaze = do
  Config {..} <- ask
  maze <- get
  let mazePicture = map (drawNode mazeSize lineLength withColor) (mazeToList maze)
  if solve
    then do
      solution <- snd <$> listen Solve.solve
      return $ Gloss.Pictures (mazePicture ++ [drawSolution lineLength solution])
    else return $ Gloss.Pictures mazePicture

drawNode :: Int -> Float -> Bool -> Node (Maybe Int) Maze.Path -> Gloss.Picture
drawNode size lineLength withColor (Node (NodeID (x, y)) val n s e w) =
  Gloss.Pictures $
    map (translation x y) pictures
  where
    edges = drawEdges lineLength (n, s, e, w)
    colorN = colorNode size lineLength val
    translation a b = translate (fromIntegral a * lineLength) (fromIntegral b * lineLength)
    pictures = if withColor then colorN : edges else edges

drawSolution :: Float -> [NodeID] -> Gloss.Picture
drawSolution lineLength solution = Gloss.Color Gloss.red $ Gloss.line $ map (pos . toPath) solution
  where
    pos (x, y) = (x * lineLength + lineLength / 2, y * lineLength + lineLength / 2)
    toPath (NodeID (x, y)) = (fromIntegral x, fromIntegral y)

drawEdges :: Float -> Edges Maze.Path -> [Gloss.Picture]
drawEdges lineLength (n, s, e, w) =
  [ drawEdge n [(0, lineLength), (lineLength, lineLength)],
    drawEdge s [(0, 0), (lineLength, 0)],
    drawEdge e [(lineLength, 0), (lineLength, lineLength)],
    drawEdge w [(0, 0), (0, lineLength)]
  ]

colorNode :: Int -> Float -> Maybe Int -> Gloss.Picture
colorNode size ll (Just val) = translate (ll / 2) (ll / 2) $ Gloss.color (scaleColor size (fromIntegral val)) $ Gloss.rectangleSolid ll ll
colorNode _ ll Nothing = translate (ll / 2) (ll / 2) $ Gloss.color Gloss.black $ Gloss.rectangleSolid ll ll

greenSmoke = Gloss.makeColorI 113 123 112 1

scaleColor :: Int -> Float -> Gloss.Color
scaleColor size factor = mixColors (fromIntegral size * 2) factor greenSmoke Gloss.black

drawEdge :: Maybe (Maze.Edge Maze.Path) -> Gloss.Path -> Gloss.Picture
drawEdge Nothing p = Gloss.Line p
drawEdge (Just (Edge _ e)) path = case e of
  Open   -> Gloss.Blank
  Closed -> Gloss.Line path

-- TODO look into simulate from Gloss to show generation steps
