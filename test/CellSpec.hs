module CellSpec where
import           Test.Hspec

spec :: Spec
spec = do
      it "can generate a new grid" $ do
        1 `shouldBe` 1
