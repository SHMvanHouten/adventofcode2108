module AOC.Day22.SpelunkingSpec where

import Test.Hspec
import Test.QuickCheck

import AOC.Day22.Spelunking
import AOC.Util.Coordinate

import qualified Data.Map as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "challenge" $ do
    it "solves challenge " $ do
      let depth = 11109
      let target = Coordinate 9 731
      determineRisk (Coordinate 0 0) target depth `shouldBe` 7299

  describe "determineRisk" $ do
    it "determines the risk for 10, 10" $ do
      determineRisk (Coordinate 0 0) (Coordinate 10 10) 510 `shouldBe` 114
    it "determines the risk for 3, 3" $ do
      determineRisk (Coordinate 0 0) (Coordinate 3 3) 510 `shouldBe` 15

  describe "determineGeologicIndex" $ do
   it "0,0 has geoIndex 0" $ do
    determineGeologicIndex (Coordinate 0 0) Map.empty `shouldBe` 0
   it "a coordinate with y 0 has geoIndex of it's x times 16807" $ do
    determineGeologicIndex (Coordinate 1 0) Map.empty `shouldBe` 16807
    determineGeologicIndex (Coordinate 2 0) Map.empty `shouldBe` (2 * 16807)
   it "a coordinate with x 0 has geoIndex of it's x times 16807" $ do
    determineGeologicIndex (Coordinate 0 1) Map.empty `shouldBe` 48271
    determineGeologicIndex (Coordinate 0 2) Map.empty `shouldBe` (2 * 48271)
   it "all other coordinates have a geoIndex of product of the erosion levels of the regions to the left and above" $ do
    let geoMap = Map.fromList [((Coordinate 0 1), 8415), ((Coordinate 1 0), 17317)]
    determineGeologicIndex (Coordinate 1 1) geoMap `shouldBe` 145722555


--The region at 0,0 (the mouth of the cave) has a geologic index of 0.
--The region at the coordinates of the target has a geologic index of 0.
--If the region's Y coordinate is 0, the geologic index is its X coordinate times 16807.
--If the region's X coordinate is 0, the geologic index is its Y coordinate times 48271.
--Otherwise, the region's geologic index is the result of multiplying the erosion levels of the regions at X-1,Y and X,Y-1.