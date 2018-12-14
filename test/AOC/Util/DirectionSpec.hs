module AOC.Util.DirectionSpec where

import Test.Hspec
import Test.QuickCheck
import AOC.Util.Direction

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "turn left" $ do
    it "faces North after turning left from facing east" $ do
      turnLeft East`shouldBe` North
    it "faces West after turning left from facing east" $ do
      turnLeft North `shouldBe` West

  describe "turn right" $ do
    it "faces West after turning right from facing South" $ do
      turnRight South `shouldBe` West
    it "faces North after turning right from facing West" $ do
      turnRight West `shouldBe` North