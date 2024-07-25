module Main where
import           BinaryTree (binaryMaze)
import           Cell       (initBlankSquareMaze, renderMaze)


main :: IO ()
main = do
  maze <- binaryMaze $ initBlankSquareMaze 15
  putStrLn $ renderMaze maze
