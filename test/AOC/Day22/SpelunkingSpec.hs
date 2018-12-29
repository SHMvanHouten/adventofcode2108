module AOC.Day22.SpelunkingSpec where

import Test.Hspec
import Test.QuickCheck

import AOC.Day22.Spelunking
import AOC.Util.Coordinate

import qualified Data.Map as Map
import qualified Data.Sequence as Seq

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "challenge part 2" $ do
    it "solves challenge part 2" $ do
      let depth = 11109
      let target = Coordinate 9 731
      let state = buildState depth target
      let initialNode = Node (Coordinate 0 0) stubNode Torch 0
      let quickestNode = findQuickestPath state (Seq.singleton initialNode) (unfoundTargetNode target)
      currentTime quickestNode `shouldBe` 1008
  describe "testInput part 2" $ do
    it "solves testInput part 2" $ do
      let depth = 510
      let target = Coordinate 10 10
      let state = buildState depth target
      let initialNode = Node (Coordinate 0 0) stubNode Torch 0
      let quickestNode = findQuickestPath state (Seq.singleton initialNode) (unfoundTargetNode target)
      currentTime quickestNode `shouldBe` 45

  describe "getSurroundingRegions" $ do
    it "gets the surrounding regions" $ do
      let regionMap = Map.fromList [((Coordinate 1 0), Wet), ((Coordinate 0 1), Rocky), ((Coordinate 2 1), Rocky), ((Coordinate 1 2), Rocky)]
      getSurroundingRegions (Coordinate 1 1) regionMap `shouldBe` [((Coordinate 1 0), Wet), ((Coordinate 0 1), Rocky), ((Coordinate 2 1), Rocky), ((Coordinate 1 2), Rocky)]

  describe "stepIsAllowed" $ do
    it "is not allowed to move to a narrow region with climbing gear" $ do
      stepIsAllowed (Coordinate 0 0, Narrow) ClimbingGear `shouldBe` False
    it "is allowed to move to a narrow region with a torch" $ do
      stepIsAllowed (Coordinate 0 0, Narrow) Torch `shouldBe` True
    it "is not allowed to move to a wet region with a torch" $ do
      stepIsAllowed (Coordinate 0 0, Wet) Torch `shouldBe` False
    it "is allowed to move to a wet region with neither gear equipped" $ do
      stepIsAllowed (Coordinate 0 0, Wet) Neither `shouldBe` True
    it "is not allowed to move to a rocky region with neither gear equipped" $ do
      stepIsAllowed (Coordinate 0 0, Rocky) Neither `shouldBe` False
    it "is allowed to move to a rocky region with climbing gear equipped" $ do
      stepIsAllowed (Coordinate 0 0, Wet) ClimbingGear `shouldBe` True

  -----------------
  --  part 1
  -----------------
  describe "challenge part 1" $ do
    it "solves challenge part 1" $ do
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
   it "a coordinate with y 0 has geoIndex of its x times 16807" $ do
    determineGeologicIndex (Coordinate 1 0) Map.empty `shouldBe` 16807
    determineGeologicIndex (Coordinate 2 0) Map.empty `shouldBe` (2 * 16807)
   it "a coordinate with x 0 has geoIndex of its x times 16807" $ do
    determineGeologicIndex (Coordinate 0 1) Map.empty `shouldBe` 48271
    determineGeologicIndex (Coordinate 0 2) Map.empty `shouldBe` (2 * 48271)
   it "all other coordinates have a geoIndex of product of the erosion levels of the regions to the left and above" $ do
    let geoMap = Map.fromList [((Coordinate 0 1), 8415), ((Coordinate 1 0), 17317)]
    determineGeologicIndex (Coordinate 1 1) geoMap `shouldBe` 145722555