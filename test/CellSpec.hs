module CellSpec where
import           Test.Hspec

import           Cell       (BoundaryType (AdjacentCell, Wall, WorldBoundary),
                             CellBoundaries (CellBoundaries),
                             Direction (Down, Left, Right, Up), boundaryToChar,
                             cellToASCII, coordsToDirection,
                             verticalBoundaryToChar)
import           Grid       (Coord (Coord))
import           Prelude    hiding (Left, Right)

spec :: Spec
spec = do
      it "convert a cell boundary to a string - horizontal" $ do
        boundaryToChar Wall `shouldBe` "---+"
        boundaryToChar WorldBoundary `shouldBe` "---+"
        boundaryToChar (AdjacentCell (Coord(0,0))) `shouldBe` "   +"

      it "convert a cell boundary to a string - vertical" $ do
        verticalBoundaryToChar Wall `shouldBe` '|'
        verticalBoundaryToChar WorldBoundary `shouldBe` '|'
        verticalBoundaryToChar (AdjacentCell (Coord(0,0))) `shouldBe` ' '

      it "returns cell boundaries for the middle and bottom of each cell" $ do
        cellToASCII (CellBoundaries WorldBoundary WorldBoundary WorldBoundary WorldBoundary (Coord(0,0))) `shouldBe` ("|   |", "+---+")
        cellToASCII (CellBoundaries WorldBoundary (AdjacentCell undefined) WorldBoundary WorldBoundary (Coord(0,0))) `shouldBe` ("|   |", "+   +")
        cellToASCII (CellBoundaries WorldBoundary (AdjacentCell undefined) Wall WorldBoundary (Coord(0,0))) `shouldBe` ("   |", "   +")
        cellToASCII (CellBoundaries WorldBoundary (AdjacentCell undefined) (AdjacentCell undefined) (AdjacentCell undefined) (Coord(0,0))) `shouldBe` ("    ", "   +")

      it "returns the directions for two given coordinates" $ do
        coordsToDirection (Coord(0,0)) (Coord(1,0)) `shouldBe` Down
        coordsToDirection (Coord(1,0)) (Coord(0,0)) `shouldBe` Up
        coordsToDirection (Coord(0,0)) (Coord(0,1)) `shouldBe` Right
        coordsToDirection (Coord(0,1)) (Coord(0,0)) `shouldBe` Left
