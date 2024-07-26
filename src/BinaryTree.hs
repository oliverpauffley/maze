-- | Generates a maze using the binary tree algorithm
module BinaryTree where

import           Cell                 (BoundaryType (AdjacentCell, Wall),
                                       CellBoundaries (..))
import           Control.Monad.Random
import qualified Data.Map.Strict      as Map
import           Grid                 (Coord (Coord), Grid (Grid))

-- | Generate a new maze from a blank grid. It does this by selecting the the cell next door that is either down or right. If they are possible
-- moves then we pick one at random and clear the wall between the squares.
binaryMaze :: (MonadRandom m) => Grid CellBoundaries -> m (Grid CellBoundaries)
binaryMaze (Grid grid) = do
  updatedPairs <- mapM updateCell (Map.assocs grid)
  return $ Grid (Map.fromList updatedPairs)
  where
    updateCell :: (MonadRandom m) => (Coord, CellBoundaries) -> m (Coord, CellBoundaries)
    updateCell (coord, cell) = do
      newCell <- binaryCell cell
      return (coord, newCell)

binaryCell :: (MonadRandom m) => CellBoundaries -> m CellBoundaries
binaryCell c@(CellBoundaries _ down _ right (Coord (y, x))) = do
  let choices = [dir | (cond, dir) <- [(down == Wall, "down"), (right == Wall, "right")], cond]
  if null choices
    then return c
    else do
      ran <- uniform choices
      return $ case ran of
        "down"  -> c {downBoundary = AdjacentCell (Coord (y - 1, x))}
        "right" -> c {rightBoundary = AdjacentCell (Coord (y, x + 1))}
        _       -> c -- This case is unnecessary since uniform ensures one of the list items is chosen
