-- | Holds information for maze spaces

module Cell where
import           Data.Maybe (catMaybes)
import           Grid       (Grid, Location, getCell, getNeighbours)

data BoundaryType = WorldBoundary | Wall | AdjacentCell Location
  deriving (Eq, Show)

data CellBoundaries = CellBoundaries
  { upBoundary    :: BoundaryType
  , downBoundary  :: BoundaryType
  , leftBoundary  :: BoundaryType
  , rightBoundary :: BoundaryType
  , location      ::Location -- this cells location
  } deriving (Eq, Show)

linkedCells :: CellBoundaries -> [Location]
linkedCells (CellBoundaries u d l r _) = catMaybes [ul, dl, ll, rl]
  where
    boundaryToLocation (AdjacentCell location) = Just location
    boundaryToLocation _                       = Nothing
    ul = boundaryToLocation u
    dl = boundaryToLocation d
    ll = boundaryToLocation l
    rl = boundaryToLocation r

isLinked :: CellBoundaries -> Location -> Bool
isLinked c l = elem l $ linkedCells c

-- linkCells :: Grid CellBoundaries -> Location -> Location -> Grid CellBoundaries
-- linkCells grid a b= do
--   cellA <- getCell a
--   cellB <- getCell b
--   -- check that they are actually neighbours
--   if elem cellB $ getNeighbours a then
--       let
--         cellA = linkCell a b


linkCell :: CellBoundaries -> CellBoundaries -> CellBoundaries
linkCell = undefined
