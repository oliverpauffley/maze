-- | Generates a grid to traverse for maze creation
module Grid where

import           Control.Monad          (guard)
import qualified Data.List.NonEmpty     as NE
import qualified Text.PrettyPrint.Boxes as B

newtype Grid a = Grid { toNonEmptyLists :: NE.NonEmpty (NE.NonEmpty a) }
    deriving (Eq, Show)

-- Smart constructor for Grid
mkGrid :: NE.NonEmpty (NE.NonEmpty a) -> Maybe (Grid a)
mkGrid rows = do
    let n = NE.length (NE.head rows)
    guard (all (\row -> NE.length row == n) rows) -- Ensure all rows have the same length
    return (Grid rows)

getDims :: Grid a -> (Int, Int)
getDims (Grid rows)= do
  let x = NE.length (NE.head rows)
  let y = NE.length rows
  (x, y)

getCell :: Grid a -> (Int, Int) -> Maybe a
getCell g@(Grid rows) (a, b)
  | a >= fst (getDims g) = Nothing
  | b >= snd (getDims g) = Nothing
  | a < 0  = Nothing
  | b < 0 = Nothing
  | otherwise = Just $ (rows NE.!! a) NE.!! b

-- Convert Grid to Box for pretty printing
gridToBox :: Show a => Grid a -> B.Box
gridToBox (Grid rows) = B.vsep 1 B.left (fmap rowToBox rows)
  where
    rowToBox :: Show a => NE.NonEmpty a -> B.Box
    rowToBox = B.hsep 2 B.top . fmap (B.text . show)

-- Pretty print Grid
prettyPrintGrid :: Show a => Grid a -> String
prettyPrintGrid = B.render . gridToBox

exampleGrid :: Grid Integer
exampleGrid =
  let (Just rows ) = mkGrid ((1 NE.:| [2, 3]) NE.:| [(4 NE.:| [5, 6]), (7 NE.:| [8, 9])])
  in
      rows
