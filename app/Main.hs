module Main where
import           BinaryTree (binaryMaze)
import           Cell       (initBlankSquareMaze, renderMaze)


main :: IO ()
main = do
  maze <- binaryMaze $ initBlankSquareMaze 12
  putStrLn $ renderMaze maze
