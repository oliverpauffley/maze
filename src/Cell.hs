-- | Holds information for maze spaces

module Cell where
import           Data.Maybe (catMaybes)
import           Grid       (Location)

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
