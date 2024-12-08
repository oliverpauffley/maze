module Main where

import           BinaryTree           (generate, generateMaze)
import           Control.Monad.Random
import           Graphics.Gloss
import           Maze

lineLength :: Float
lineLength = 50

mazeSize :: Int
mazeSize = 12

startWindowPos :: Float -> Int -> (Int, Int)
startWindowPos len size =
  ( (size * round len) `div` 2,
    (size * round len) `div` 2
  )

main :: IO ()
main = do
  let maze = newMaze mazeSize
  generated <- BinaryTree.generateMaze maze
  display (InWindow "Maze" (100, 100) (startWindowPos lineLength mazeSize)) white (drawMaze generated)

drawMaze :: Maze -> Picture
drawMaze maze =
  Pictures $ map drawNode (mazeToList maze)

drawNode :: Node () Maze.Path -> Picture
drawNode (Node (x, y) _ n s e w) =
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
