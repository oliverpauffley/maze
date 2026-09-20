import MazeShape (GridShape (neighbor))
import qualified MazeShape.Sigma as Sigma
import Test.Hspec
import qualified MazeShape as Sigma

main :: IO ()
main = hspec $ do
    describe "Maze" $ do
      sigmaSpec

sigmaSpec :: Spec
sigmaSpec =
      describe "Sigma" $ do
         neighbourSpec
         neighboursSpec

neighboursSpec :: Spec
neighboursSpec =
  describe "neigbours" $ do
     it "gives neighbours for even nodes" $ do
       Sigma.neighbors (Sigma.Sigma (2,1)) `shouldBe` Sigma.Sigma <$>
         [(2,0)
         ,(2,2)
         ,(3,0)
         ,(1,0)
         ,(3,1)
         ,(1,1)]
     it "gives neighbours for odd nodes" $ do
       Sigma.neighbors (Sigma.Sigma (1,1)) `shouldBe` Sigma.Sigma <$>
         [(1,0)
         ,(1,2)
         ,(2,1)
         ,(0,1)
         ,(2,2)
         ,(0,2)]

neighbourSpec :: Spec
neighbourSpec =
      describe "neighbour" $ do
        context "when moving South" $ do
          it "works when moving from even nodes" $ do
              Sigma.neighbor (Sigma.Sigma (0, 0)) Sigma.South `shouldBe` Just (Sigma.Sigma (0, 1))
          it "works when moving from odd nodes" $ do
            Sigma.neighbor (Sigma.Sigma (1, 0)) Sigma.South `shouldBe` Just (Sigma.Sigma (1, 1))
        context "when moving North" $ do
          it "works when moving from even nodes" $ do
            Sigma.neighbor (Sigma.Sigma (0,1)) Sigma.North `shouldBe` Just (Sigma.Sigma (0,0))
          it "works when moving from odd nodes" $ do
            Sigma.neighbor (Sigma.Sigma (1,1)) Sigma.North `shouldBe` Just (Sigma.Sigma (1,0))

        context "when moving NorthEast" $ do
          it "works when moving from even nodes" $ do
            Sigma.neighbor (Sigma.Sigma (0,1)) Sigma.NorthEast `shouldBe` Just (Sigma.Sigma (1,0))
          it "works when moving from odd nodes" $ do
            Sigma.neighbor (Sigma.Sigma (1,1)) Sigma.NorthEast `shouldBe` Just (Sigma.Sigma (2,1))
        context "when moving NorthWest" $ do
          it "works when moving from even nodes" $ do
            neighbor (Sigma.Sigma (2,1)) Sigma.NorthWest `shouldBe` Just (Sigma.Sigma (1,0))
          it "works when moving from odd nodes" $ do
            neighbor (Sigma.Sigma (1,1)) Sigma.NorthWest `shouldBe` Just (Sigma.Sigma (0,1))
        context "when moving SouthEast" $ do
          it "works when moving from even nodes" $ do
            neighbor (Sigma.Sigma (2,1)) Sigma.SouthEast `shouldBe` Just (Sigma.Sigma (3,1))
          it "works when moving from odd nodes" $ do
            neighbor (Sigma.Sigma (1,1)) Sigma.SouthEast `shouldBe` Just (Sigma.Sigma (2,2))
        context "when moving SouthWest" $ do
          it "works when moving from even nodes" $ do
            neighbor (Sigma.Sigma (2,1)) Sigma.SouthWest `shouldBe` Just (Sigma.Sigma (1,1))
          it "works when moving from odd nodes" $ do
            neighbor (Sigma.Sigma (1,1)) Sigma.SouthWest `shouldBe` Just (Sigma.Sigma (0,2))
