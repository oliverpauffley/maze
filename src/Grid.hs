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

-- Convert Grid to Box for pretty printing
gridToBox :: Show a => Grid a -> B.Box
gridToBox (Grid rows) = B.vsep 1 B.left (map rowToBox (NE.toList rows))
  where
    rowToBox :: Show a => NE.NonEmpty a -> B.Box
    rowToBox = B.hsep 2 B.top . map (B.text . show) . NE.toList

-- Pretty print Grid
prettyPrintGrid :: Show a => Maybe (Grid a) -> String
prettyPrintGrid (Just grid) = (B.render . gridToBox) grid
prettyPrintGrid Nothing     = ""

exampleGrid :: Maybe (Grid Integer)
exampleGrid = mkGrid ((1 NE.:| [2, 3]) NE.:| [(4 NE.:| [5, 6]), (7 NE.:| [8, 9])])
