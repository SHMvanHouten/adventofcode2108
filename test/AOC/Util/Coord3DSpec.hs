module AOC.Util.Coord3DSpec where

import AOC.Util.Coord3D

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "distance" $ do
    it "distance between 0 0 0 and 3 3 3 is 9" $ do
      let coord1 = Coord3d 0 0 0
      let coord2 = Coord3d 3 3 3
      distance coord1 coord2 `shouldBe` 9
      distance coord2 coord1 `shouldBe` 9
    it "distance between 1 2 3 and 3 2 1 is 4" $ do
      let coord1 = Coord3d 1 2 3
      let coord2 = Coord3d 3 2 1
      distance coord1 coord2 `shouldBe` 4
      distance coord2 coord1 `shouldBe` 4
    it "distance between -1 2 -3 and 1 -2 -4 is 7" $ do
      let coord1 = Coord3d (-1) 2 (-3)
      let coord2 = Coord3d 1 (-2) (-4)
      distance coord1 coord2 `shouldBe` 7
      distance coord2 coord1 `shouldBe` 7

