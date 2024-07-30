{-# LANGUAGE OverloadedStrings #-}

-- | Holds information for maze spaces
module Cell where

import           Data.Foldable   (foldl')
import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes, fromMaybe)
import           Grid            (Coord (Coord), Grid (Grid), getCell, getDims,
                                  insertElem)
import           Prelude         hiding (Left, Right)

data BoundaryType =  WorldBoundary | Wall | AdjacentCell Coord
  deriving (Eq, Show, Ord)

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
    boundaryToLocation (AdjacentCell loc) = Just loc
    boundaryToLocation _                  = Nothing
    ul = boundaryToLocation u
    dl = boundaryToLocation d
    ll = boundaryToLocation l
    rl = boundaryToLocation r

isLinked :: CellBoundaries -> Coord -> Bool
isLinked c l = elem l $ linkedCells c

-- | from the given cell find all horizontally linked cells
getLinkedRowCells :: CellBoundaries -> [Coord]
getLinkedRowCells cell = [x | x <- linkedCells cell, isOnSameRow x (location cell)]

isOnSameRow :: Coord -> Coord -> Bool
isOnSameRow (Coord (y, _)) (Coord (y2, _))
  | y == y2 = True
  | otherwise = False

linkCells :: Maze -> (Coord, Coord) -> Maze
linkCells maze (a, b) = do
  let
    mazeA = linkCell maze a b
    mazeB = linkCell mazeA b a
  mazeB

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


fromListMerge :: (a -> a -> a) -> [(Coord, a)] -> Grid a
fromListMerge func xs = Grid $ foldl' ins Map.empty xs
  where
    ins t (k,x) = Map.insertWith func k x t

mergeCells :: CellBoundaries -> CellBoundaries -> CellBoundaries
mergeCells (CellBoundaries ua da la ra (Coord (ya, xa))) (CellBoundaries ub db lb rb _) =
  CellBoundaries
   (max ua ub)
   (max da db)
   (max la lb)
   (max ra rb)
   (Coord (ya,xa))

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
