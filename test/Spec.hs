import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "maze" $ do
    it "creates a new blank maze" $
      True `shouldBe` True
