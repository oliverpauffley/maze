-- | Generates a grid to traverse for maze creation
-- TODO change to map not list
module Grid where

import           Control.Monad          (guard)
import qualified Data.List.NonEmpty     as NE
import           Data.Maybe             (catMaybes, isNothing)
import qualified Text.PrettyPrint.Boxes as B

newtype Location = Location (Int, Int)
  deriving (Eq, Show)

-- |a 2d grid of values used to construct mazes. Expects a nested array of non-empty lists.
newtype Grid a = Grid { toNonEmptyLists :: NE.NonEmpty (NE.NonEmpty a) }
    deriving (Eq, Show)

-- Smart constructor for Grid
mkGrid :: NE.NonEmpty (NE.NonEmpty a) -> Maybe (Grid a)
mkGrid rows = do
    let n = NE.length (NE.head rows)
    guard (all (\row -> NE.length row == n) rows) -- Ensure all rows have the same length
    return (Grid rows)

getDims :: Grid a -> (Int, Int)
getDims (Grid rows) = do
  let x = NE.length (NE.head rows)
  let y = NE.length rows
  (x, y)

getCell :: Grid a -> Location -> Maybe a
getCell g@(Grid rows) (Location(a, b))
  | a >= fst (getDims g) = Nothing
  | b >= snd (getDims g) = Nothing
  | a < 0  = Nothing
  | b < 0 = Nothing
  | otherwise = Just $ (rows NE.!! a) NE.!! b

getNeighbours :: Grid a -> Location -> [a]
getNeighbours g (Location (a, b)) =
  if isNothing $ getCell g (Location(a, b)) then []
  else
  let
        up = getCell g (Location(a+1, b))
        down = getCell g (Location(a-1, b))
        right = getCell g (Location(a, b+1))
        left = getCell g (Location(a, b-1))
  in
        catMaybes [right,down,left,up]

insertElem :: Grid a -> a -> Location -> Grid a
insertElem grid elem location =

-- Convert Grid to Box for pretty printing
gridToBox :: Show a => Grid a -> B.Box
gridToBox (Grid rows) = B.vsep 1 B.left (fmap rowToBox rows)
  where
    rowToBox :: Show a => NE.NonEmpty a -> B.Box
    rowToBox = B.hsep 2 B.top . fmap (B.text . show)

-- Pretty print Grid
prettyPrintGrid :: Show a => Grid a -> String
prettyPrintGrid = B.render . gridToBox

instance Functor Grid where
  fmap f (Grid rows) = Grid $ (fmap . fmap) f rows
