module Main where
import           Cell       (initBlankSquareMaze, renderMaze)
import           Sidewinder (sidewinderMaze)


main :: IO ()
main = do
  maze <- sidewinderMaze $ initBlankSquareMaze 12
  putStrLn $ renderMaze maze
