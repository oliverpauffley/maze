{-# LANGUAGE OverloadedStrings #-}

-- | Holds information for maze spaces
module Cell where

import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes, fromMaybe)
import           Grid            (Coord (Coord), Grid (Grid), getCell, getDims,
                                  insertElem)
import           Prelude         hiding (Left, Right)

-- TODO this should be linked to cells not coordinates
data BoundaryType =  WorldBoundary | Wall | AdjacentCell CellBoundaries
  deriving (Eq, Show)

data Direction = Up | Down | Left | Right
  deriving (Eq, Show, Ord)

data CellBoundaries = CellBoundaries
  { upBoundary    :: BoundaryType,
    downBoundary  :: BoundaryType,
    leftBoundary  :: BoundaryType,
    rightBoundary :: BoundaryType,
    location      :: Coord
  }
  deriving (Eq, Show)

type Maze = Grid CellBoundaries

-- | for given dimensions and a cell return the correct cell boundaries. This defaults to `Wall`s between cells and
-- `WorldBoundary` at the edges.
startBoundaries :: (Int, Int) -> Coord -> CellBoundaries
startBoundaries (numCols, numRows) (Coord (y, x)) =
  CellBoundaries
    (if y > 0 then Wall else WorldBoundary)
    ( if y + 1 < numCols
        then Wall
        else WorldBoundary
    )
    (if x > 0 then Wall else WorldBoundary)
    ( if x + 1 < numRows
        then Wall
        else WorldBoundary
    )
    (Coord (y, x))

initBlankSquareMaze :: Int -> Maze
initBlankSquareMaze size =
  Grid $
    Map.fromList
      [ (Coord (i, j), startSquare (Coord (i, j)))
        | i <- [0 .. size - 1],
          j <- [0 .. size - 1]
      ]
  where
    startSquare = startBoundaries (size, size)

linkedCells :: CellBoundaries -> [Coord]
linkedCells (CellBoundaries u d l r _) = catMaybes [ul, dl, ll, rl]
  where
    ul = boundaryToLocation u
    dl = boundaryToLocation d
    ll = boundaryToLocation l
    rl = boundaryToLocation r

boundaryToLocation :: BoundaryType -> Maybe Coord
boundaryToLocation (AdjacentCell loc) = Just loc
boundaryToLocation _                  = Nothing

isLinked :: CellBoundaries -> Coord -> Bool
isLinked c l = elem l $ linkedCells c

-- -- | from the given cell find all horizontally linked cells
-- getLinkedRowCells :: Grid CellBoundaries -> CellBoundaries -> [Coord]
-- getLinkedRowCells maze (CellBoundaries _ _ l r _)  =
--   where
--         getAllLeftCells = foldl' ()

--         getLeftCell :: [Coord] -> Ce

isOnSameRow :: Coord -> Coord -> Bool
isOnSameRow (Coord (y, _)) (Coord (y2, _))
  | y == y2 = True
  | otherwise = False

linkCells :: Maze -> (Coord, Coord) -> Maze
linkCells maze (a, b) = linkCell (linkCell maze a b) b a

linkCell :: Maze -> Coord -> Coord -> Maze
linkCell maze a b = do
  case getCell maze a of
    Just cellA -> do
      let
        newCellA = addCellLink cellA b
      insertElem maze a newCellA
    Nothing -> maze


addCellLink :: CellBoundaries -> Coord -> CellBoundaries
addCellLink cell coord =
  case coordsToDirection (location cell) coord of
    Left  -> cell {leftBoundary = AdjacentCell coord}
    Right -> cell {rightBoundary = AdjacentCell coord}
    Up    -> cell {upBoundary = AdjacentCell coord}
    Down  -> cell {downBoundary = AdjacentCell coord}

coordsToDirection :: Coord -> Coord -> Direction
coordsToDirection (Coord(y1, x1)) (Coord(y2, x2))
  | y1 == y2 && x1 > x2 = Left
  | y1 == y2 && x1 < x2 = Right
  | x1 == x2 && y1 > y2 = Up
  | otherwise = Down


boundaryToChar :: BoundaryType -> String
boundaryToChar Wall             = "---+"
boundaryToChar WorldBoundary    = "---+"
boundaryToChar (AdjacentCell _) = "   +"

verticalBoundaryToChar :: BoundaryType -> Char
verticalBoundaryToChar Wall             = '|'
verticalBoundaryToChar WorldBoundary    = '|'
verticalBoundaryToChar (AdjacentCell _) = ' '

cellToASCII :: CellBoundaries -> (String, String)
cellToASCII (CellBoundaries _ down left right _) =
  let -- Determining the edges
      leftEdge WorldBoundary = "|"
      leftEdge _             = ""
      leftBottom WorldBoundary = "+"
      leftBottom _             = ""

      midSection = leftEdge left ++ "   " ++ [verticalBoundaryToChar right]
      bottomEdge = leftBottom left ++ boundaryToChar down
   in (midSection, bottomEdge)

renderMaze :: Maze -> String
renderMaze m@(Grid cells) = unlines (topLine : concatMap tupleToLines rows)
  where
    (maxCol, maxRow) = getDims m

    -- Create list of list of (String, String, String) for each cell in the grid
    asciiCellsList = [[cellToASCII $ fromMaybe emptyCell (Map.lookup (Coord (r, c)) cells) | r <- [0 .. maxCol - 1]] | c <- [0 .. maxRow - 1]]

    -- Transpose the list to produce ASCII strings by rows
    rows = foldr (zipWith appendTuple) (replicate maxRow ("", "")) asciiCellsList

    -- Convert each triplet of lines into a flat list of lines
    tupleToLines (a, b) = [a, b]

    appendTuple (a1, a2) (r1, r2) = (a1 ++ r1, a2 ++ r2)

    -- Prepare the empty cell for out-of-bound lookups
    emptyCell = CellBoundaries WorldBoundary WorldBoundary WorldBoundary WorldBoundary (Coord (0, 0))

    -- topline is the first line of the maze (always closed)
    topLine = "+" ++ concat (replicate maxRow "---+")
