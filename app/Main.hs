module Main where

import           BinaryTree                       (generateMaze)
import           Control.Monad.Trans.State.Strict
import           Graphics.Gloss
import           Maze
import qualified Sidewinder

lineLength :: Float
lineLength = 50

mazeSize :: Int
mazeSize = 20

startWindowPos :: Float -> Int -> (Int, Int)
startWindowPos len size =
  ( (size * round len) `div` 4,
    (size * round len) `div` 4
  )

main :: IO ()
main = do
  (_, maze) <- runStateT Sidewinder.generateMaze (newMaze mazeSize)
  print maze
  display (InWindow "Maze" (100, 100) (startWindowPos lineLength mazeSize)) white (drawMaze maze)

drawMaze :: Maze -> Picture
drawMaze maze =
  Pictures $ map drawNode (mazeToList maze)

drawNode :: Node () Maze.Path -> Picture
drawNode (Node (NodeID (x, y)) _ n s e w) =
  Pictures $
    map
      ( translate
          (fromIntegral x * lineLength)
          (fromIntegral y * lineLength)
      )
      ( drawEdges
          (n, s, e, w)
          -- if you want the grid positions
          -- ++ [ translate 20 20 $
          --        scale 0.10 0.10 $
          --          text
          --            ( "(" ++ show x ++ "," ++ show y ++ ")"
          --            )
          --    ]
      )

drawEdges :: Edges Maze.Path -> [Picture]
drawEdges (n, s, e, w) =
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
