-- | Generates a maze using the sidewinder algorithm. Similar to
-- the binary tree we either go right or down but when we move down we randomly
-- select one of the previous cut row to choose from.
module Sidewinder where

import           Cell                 (BoundaryType (Wall),
                                       CellBoundaries (CellBoundaries, location),
                                       Direction (Down, Right), Maze,
                                       getLinkedRowCells, linkCells,
                                       linkedCells, renderMaze)
import           Control.Monad.Random (MonadRandom, uniform)
import           Data.Foldable        (foldlM)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe)
import           Debug.Trace
import           Grid                 (Coord (Coord), Grid (Grid), getCell)
import           Prelude              hiding (Right)


sidewinderMaze :: (MonadRandom m) => Maze -> m Maze
sidewinderMaze maze@(Grid grid) =
  foldlM sidewinderCell maze cells
  where
    cells :: [CellBoundaries]
    cells = snd <$> Map.toAscList grid

sidewinderCell :: (MonadRandom m) => Maze -> CellBoundaries -> m Maze
sidewinderCell maze c@(CellBoundaries _ down _ right (Coord (y, x))) =
  pickDirection choices
  where

    choices = [dir | (cond, dir) <- [(down == Wall, Down), (right == Wall, Right)], cond]

    pickDirection [] = return maze
    pickDirection directions = do
      ran <- uniform directions
      case ran of
        Down  -> handleDown c
        Right -> return $ linkCells maze (Coord (y, x), Coord (y, x + 1))
        _     -> undefined

    handleDown currentCell = do
          let
            choice = getLinkedRowCells currentCell ++ [location currentCell]
            --(Coord(currentY, currentX))= location currentCell
           -- Just leftCell = getCell maze (Coord(currentY, currentX -1))
          newCell <- (trace $ (show . linkedCells ) currentCell ) (trace $ show $ location currentCell) (trace $ renderMaze maze) uniform choice
          let cellM = getCell maze newCell
              cell = fromMaybe undefined cellM
              Coord (newY, newX) = location cell
          return $ linkCells maze (Coord (newY, newX), Coord (newY + 1, newX))
