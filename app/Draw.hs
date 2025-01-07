module Draw where

import qualified Graphics.Gloss              (Path,
                                              Picture (Blank, Line, Pictures),
                                              scale, text)
import qualified Graphics.Gloss              as Gloss
import           Graphics.Gloss.Data.Picture (translate)
import           Maze                        (Edge (Edge), Edges, Maze,
                                              Node (Node), NodeID (NodeID),
                                              Path (Closed, Open), mazeToList)
import           Param                       (Config (lineLength))

drawMaze :: Float -> Maze -> [NodeID] -> Gloss.Picture
drawMaze lineLength maze solution =
  Gloss.Pictures $ map (drawNode lineLength) (mazeToList maze) ++ [drawSolution lineLength solution]

drawNode :: Float -> Node (Maybe Int) Maze.Path -> Gloss.Picture
drawNode lineLength (Node (NodeID (x, y)) _ n s e w) =
  Gloss.Pictures $
    map
      ( translate
          (fromIntegral x * lineLength)
          (fromIntegral y * lineLength)
      )
      ( drawEdges
          lineLength
          (n, s, e, w)
      )

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

drawEdge :: Maybe (Maze.Edge Maze.Path) -> Graphics.Gloss.Path -> Gloss.Picture
drawEdge Nothing p = Gloss.Line p
drawEdge (Just (Edge _ e)) path = case e of
  Open   -> Gloss.Blank
  Closed -> Gloss.Line path

-- TODO look into simulate from Gloss to show generation steps
