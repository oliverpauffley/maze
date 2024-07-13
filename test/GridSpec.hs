{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module GridSpec (spec) where
import qualified Data.List.NonEmpty as NE
import           Grid               (Location (Location), getDims)
import           Grid               as G (Grid (Grid), getCell, getNeighbours,
                                          mkGrid)
import           Test.Hspec

spec :: Spec
spec = do
      it "can generate a new grid" $ do
        mkGrid exampleNestedNonEmpty  `shouldBe` Just (Grid exampleNestedNonEmpty)

      it "grid must be square" $ do
        mkGrid exampleNestedNonEmptyNotSquare  `shouldBe` Nothing

      it "getDims returns the dimensions of a grid" $ do
        getDims exampleGrid3x3  `shouldBe` (3,3)
        getDims exampleGrid2x2  `shouldBe` (2,2)

      it "getCells returns cells when present" $ do
        getCell exampleGrid2x2 (Location (0,0)) `shouldBe` Just 1
        getCell exampleGrid2x2 (Location (0,1)) `shouldBe` Just 2
        getCell exampleGrid2x2 (Location (1,0)) `shouldBe` Just 3
        getCell exampleGrid2x2 (Location (1,1)) `shouldBe` Just 4

      it "getCells returns Nothing outside of grid" $ do
        getCell exampleGrid2x2 (Location (4,4)) `shouldBe` Nothing
        getCell exampleGrid2x2 (Location (-1,-1)) `shouldBe` Nothing

      it "GetNeigbours returns a list of neighbouring cells" $ do
        getNeighbours exampleGrid2x2 (Location (0,0)) `shouldBe` [2,3]

exampleNestedNonEmpty :: NE.NonEmpty(NE.NonEmpty Int)
exampleNestedNonEmpty = (1 NE.:| [2]) NE.:| [3 NE.:| [4]]

exampleNestedNonEmptyNotSquare :: NE.NonEmpty(NE.NonEmpty Int)
exampleNestedNonEmptyNotSquare = (1 NE.:| [2]) NE.:| [3 NE.:| [4, 5]]

exampleGrid3x3 :: Grid Integer
exampleGrid3x3 =
  let (Just rows ) = mkGrid ((1 NE.:| [2, 3]) NE.:| [4 NE.:| [5, 6], 7 NE.:| [8, 9]])
  in
      rows

exampleGrid2x2 :: Grid Integer
exampleGrid2x2 =
  let (Just rows ) = mkGrid ((1 NE.:| [2]) NE.:| [3 NE.:| [4]])
  in
      rows
