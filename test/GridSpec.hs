{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module GridSpec (spec) where

import qualified Data.Map.Strict as Map
import           Grid            (getCell, getDims, getNeighbours)
import           Grid            as G (Coord, Grid (Grid), insertElem, mkGrid)
import           Test.Hspec

spec :: Spec
spec = do
      it "can generate a new grid" $ do
        mkGrid exampleNested  `shouldBe` Just (Grid (Map.fromList [(Coord (0,0),1),(Coord (0,1),2),(Coord (1,0),3),(Coord (1,1),4)]))

      it "grid must be square" $ do
        mkGrid exampleNotSquare  `shouldBe` Nothing

      it "getDims returns the dimensions of a grid" $ do
        getDims exampleGrid3x3  `shouldBe` (3,3)
        getDims exampleGrid2x2  `shouldBe` (2,2)

      it "getCells returns cells when present" $ do
        getCell exampleGrid2x2 (Coord (0,0)) `shouldBe` Just 1
        getCell exampleGrid2x2 (Coord (0,1)) `shouldBe` Just 2
        getCell exampleGrid2x2 (Coord (1,0)) `shouldBe` Just 3
        getCell exampleGrid2x2 (Coord (1,1)) `shouldBe` Just 4

      it "getCells returns Nothing outside of grid" $ do
        getCell exampleGrid2x2 (Coord (4,4)) `shouldBe` Nothing
        getCell exampleGrid2x2 (Coord (-1,-1)) `shouldBe` Nothing

      it "getNeigbours returns a list of neighbouring cells" $ do
        getNeighbours exampleGrid2x2 (Coord (0,0)) `shouldBe` [2,3]
        getNeighbours exampleGrid2x2 (Coord (1,1)) `shouldBe` [2,3]
        getNeighbours exampleGrid2x2 (Coord (0,1)) `shouldBe` [1,4]
        getNeighbours exampleGrid2x2 (Coord (1,0)) `shouldBe` [4,1]

      it "insertElem updates an element with a new value" $ do
        getCell (insertElem exampleGrid2x2 (Coord (0,0)) 5) (0,0) `shouldBe` Just 5
        getCell (insertElem exampleGrid2x2 (Coord (1,0)) 5) (1,0) `shouldBe` Just 5

exampleNested :: [[Int]]
exampleNested = [[1, 2],[3,4]]

exampleNotSquare :: [[Int]]
exampleNotSquare = [[1, 2],[3]]

exampleGrid3x3 :: Grid Integer
exampleGrid3x3 =
  let (Just rows ) = mkGrid [[1,2,3],[4,5,6],[7,8,9]]
  in
      rows

exampleGrid2x2 :: Grid Int
exampleGrid2x2 =
  let (Just rows ) = mkGrid exampleNested
  in
      rows
