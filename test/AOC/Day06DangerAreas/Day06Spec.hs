module AOC.Day06DangerAreas.Day06Spec (spec) where

import Test.Hspec
import Test.QuickCheck


import AOC.Day06DangerAreas.LeastDangerousArea as Area


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "calulateDistanceBetweenCoordinates and center" $ do
    it "calculates a distance of 2 between 0 0 and 1 1" $ do
      calculateDistanceFromCoordinate 0 0 (Center (Coordinate 1 1) []) `shouldBe` 2
