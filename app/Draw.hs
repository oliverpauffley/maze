module Draw where

import           Graphics.Gloss              (Path,
                                              Picture (Blank, Line, Pictures))
import           Graphics.Gloss.Data.Picture (translate)
import           Maze                        (Edge (Edge), Edges, Maze,
                                              Node (Node), NodeID (NodeID),
                                              Path (Closed, Open), mazeToList)

drawMaze :: Float -> Maze -> Picture
drawMaze lineLength maze =
  Pictures $ map (drawNode lineLength) (mazeToList maze)

drawNode :: Float -> Node () Maze.Path -> Picture
drawNode lineLength (Node (NodeID (x, y)) _ n s e w) =
  Pictures $
    map
      ( translate
          (fromIntegral x * lineLength)
          (fromIntegral y * lineLength)
      )
      ( drawEdges
          lineLength
          (n, s, e, w)
          -- if you want the grid positions
          -- ++ [ translate 20 20 $
          --        scale 0.10 0.10 $
          --          text
          --            ( "(" ++ show x ++ "," ++ show y ++ ")"
          --            )
          --    ]
      )

drawEdges :: Float -> Edges Maze.Path -> [Picture]
drawEdges lineLength (n, s, e, w) =
  [ drawEdge n [(0, lineLength), (lineLength, lineLength)],
    drawEdge s [(0, 0), (lineLength, 0)],
    drawEdge e [(lineLength, 0), (lineLength, lineLength)],
    drawEdge w [(0, 0), (0, lineLength)]
  ]

drawEdge :: Maybe (Maze.Edge Maze.Path) -> Graphics.Gloss.Path -> Picture
drawEdge Nothing p = Line p
drawEdge (Just (Edge _ e)) path = case e of
  Open   -> Blank
  Closed -> Line path

-- TODO look into simulate from Gloss to show generation steps
