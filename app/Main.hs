module Main where

import           Graphics.Gloss
import           Maze           (Maze, newMaze)

main :: IO ()
main = display (InWindow "Maze" (200, 200) (10, 10)) white (drawMaze $ newMaze 10)

drawMaze :: Maze -> Picture
drawMaze =
  undefined
    -- Pictures
    -- [Color red $ Line [(0, 0), (100, 0), (100, 100), (0, 100), (0, 0)]]
