-- | Generates a maze using the sidewinder algorithm. Similar to
-- the binary tree we either go right or down but when we move down we randomly
-- select one of the previous cut row to choose from.
module Sidewinder where

import           Cell                 (BoundaryType (Wall),
                                       CellBoundaries (CellBoundaries, location),
                                       Direction (Down, Right), Maze,
                                       getLinkedRowCells, linkCells,
                                       linkedCells)
import           Control.Monad.Random (MonadRandom, uniform)
import           Data.Foldable        (foldl')
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (catMaybes, fromMaybe)
import           Grid                 (Coord (Coord), Grid (Grid), getCell)
import           Prelude              hiding (Right)

sidewinderMaze :: (MonadRandom m) => Maze -> m Maze
sidewinderMaze maze@(Grid grid) = do
  updates <- mapM (sidewinderCell maze . snd) (Map.toAscList grid)
  return $ foldl' linkCells maze $ catMaybes updates

-- returns two cells to link
sidewinderCell :: (MonadRandom m) => Maze -> CellBoundaries -> m (Maybe (Coord, Coord))
sidewinderCell maze c@(CellBoundaries _ down _ right (Coord (y, x))) =
  pickDirection choices
  where
    choices = [dir | (cond, dir) <- [(down == Wall, Down), (right == Wall, Right)], cond]
    pickDirection [] = return Nothing
    pickDirection directions = do
      ran <- uniform directions
      case ran of
        Down -> do
          choice <- uniform $ getLinkedRowCells c ++ [location c]
          let cellM = getCell maze choice
              cell = fromMaybe undefined cellM
              Coord (newY, newX) = location cell
          return $ Just (Coord (newY, newX), Coord (newY + 1, newX))
        Right -> return $ Just (Coord (y, x), Coord (y, x + 1))
        _ -> undefined
