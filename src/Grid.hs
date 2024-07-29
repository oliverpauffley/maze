{-# LANGUAGE InstanceSigs #-}
-- | Generates a grid to traverse for maze creation
module Grid where

import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes, fromMaybe, isNothing)
--import qualified Text.PrettyPrint.Boxes as B


newtype Coord = Coord (Int, Int)
  deriving (Eq)


instance Show Coord where
  show (Coord(y, x)) =  show y ++ "," ++ show x

instance (Ord Coord) where
  compare :: Coord -> Coord -> Ordering
  compare (Coord (a1, a2)) (Coord (b1, b2))
    | a1 > b1 = GT
    | a1 == b1 && a2 > b2 = GT
    | a1 == b1 && a2 == b2 = EQ
    | otherwise = LT

-- |a 2d grid of values used to construct mazes. Expects a nested array of non-empty lists.
newtype Grid a = Grid (Map.Map Coord a)
    deriving (Eq, Show)


-- Smart constructor for Grid
mkGrid :: [[a]] -> Maybe (Grid a)
mkGrid rows = do
    if all (\row -> length row == n) rows -- Ensure all rows have the same length
      then Just $ Grid (nestedListToMap rows)
    else Nothing
    where n = length (head rows)

nestedListToMap :: [[a]] -> Map.Map Coord a
nestedListToMap rows = Map.fromList [ (Coord (i, j), val)
                                    | (i, row) <- zip [0..] rows
                                    , (j, val) <- zip [0..] row ]

getDims :: Grid a -> (Int, Int)
getDims (Grid rows) =
  let Coord (x, y) = fst (fromMaybe (Coord (1,1), undefined) (Map.lookupMax rows))
  in
    (x +1, y +1)

getCell :: Grid a -> Coord -> Maybe a
getCell (Grid rows) k =
  Map.lookup k rows


-- | get the neigbouring cells in a clockwise order from east first.
getNeighbours :: Grid a -> Coord -> [a]
getNeighbours g (Coord (a, b)) =
  if isNothing $ getCell g (Coord (a, b)) then []
  else
  let
        up = getCell g (Coord (a+1, b))
        down = getCell g (Coord (a-1, b))
        right = getCell g (Coord (a, b+1))
        left = getCell g (Coord (a, b-1))
  in
        catMaybes [right,down,left,up]

insertElem :: Grid a -> Coord -> a -> Grid a
insertElem (Grid rows) k v =
  Grid $ Map.insert k v rows


-- TODO fix this
-- -- Convert Grid to Box for pretty printing
-- gridToBox :: Show a => Grid a -> B.Box
-- gridToBox (Grid rows) = B.vsep 1 B.left (fmap rowToBox rows)
--   where
--     rowToBox :: Show a => NE.NonEmpty a -> B.Box
--     rowToBox = B.hsep 2 B.top . fmap (B.text . show)

-- -- Pretty print Grid
-- prettyPrintGrid :: Show a => Grid a -> String
-- prettyPrintGrid = B.render . gridToBox

instance Functor Grid where
  fmap f (Grid rows) = Grid (fmap f rows)

instance Foldable Grid where
  foldMap f (Grid rows) = foldMap f rows

instance Traversable Grid where
  traverse :: Applicative f => (a -> f b) -> Grid a -> f (Grid b)
  traverse f (Grid rows) = Grid <$> traverse f rows
