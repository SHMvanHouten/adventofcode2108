module AOC.Day13.DeadlyRollercoasterSpec where

import Test.Hspec
import Test.QuickCheck
import AOC.Day13.DeadlyRollercoaster
import qualified Data.Map as Map
import AOC.Util.Coordinate
import AOC.Util.Direction

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "it should solve the challenge input" $ do
    it "should solve the challenge pt 2" $ do
      contents <- readFile "resources/input-day13.txt"
      let trackMapToCarts = parseRawInput contents
      crashUntilOneLasts (fst trackMapToCarts) (snd trackMapToCarts) `shouldBe` Coordinate 2 81

  describe "it should solve the challenge input" $ do
    it "should solve the challenge" $ do
      contents <- readFile "test/resources/day13-testInput2.txt"
      let trackMapToCarts = parseRawInput contents
      crashUntilOneLasts (fst trackMapToCarts) (snd trackMapToCarts) `shouldBe` Coordinate 6 4

  describe "it should solve the challenge input" $ do
    it "should solve the challenge" $ do
      contents <- readFile "resources/input-day13.txt"
      let trackMapToCarts = parseRawInput contents
      findFirstCrash (fst trackMapToCarts) (snd trackMapToCarts) `shouldBe` Coordinate 86 118

  describe "it should solve the test input" $ do
    it "the first crash should be at location 7,3" $ do
      contents <- readFile "test/resources/day13-testInput.txt"
      let trackMapToCarts = parseRawInput contents
      findFirstCrash (fst trackMapToCarts) (snd trackMapToCarts) `shouldBe` Coordinate 7 3

  describe "it should solve the test input" $ do
    it "the first crash should be at location 7,3" $ do
      let trackMapToCarts = parseRawInput "->--<-"
      findFirstCrash (fst trackMapToCarts) (snd trackMapToCarts) `shouldBe` Coordinate 3 0

  describe " parseTrackAndCart" $ do
    it "should parse the track and the carts on it" $ do
      let trackNCarts = parseRawInput "->-<-"
      fst trackNCarts `shouldBe` Map.fromList [((Coordinate 0 0), (Track (Coordinate 0 0) Horizontal)),
                                                  ((Coordinate 1 0),(Track (Coordinate 1 0) Horizontal)),
                                                  ((Coordinate 2 0),(Track (Coordinate 2 0) Horizontal)),
                                                  ((Coordinate 3 0),(Track (Coordinate 3 0) Horizontal)),
                                                  ((Coordinate 4 0),(Track (Coordinate 4 0) Horizontal))]
      snd trackNCarts `shouldBe` [(Cart (Coordinate 3 0) West L), (Cart (Coordinate 1 0) East L)]

  describe "move cart one tick" $ do
    it "should move the cart in the direction it is facing and not change direction" $ do
      contents <- readFile "test/resources/day13-testInput.txt"
      let trackMap = fst $ parseRawInput contents
      let cart = Cart (Coordinate 0 2) North L
      (moveCart cart trackMap) `shouldBe` Cart (Coordinate 0 1) North L
    it "should move the cart in the direction it is facing and turn right on the slash forward" $ do
      contents <- readFile "test/resources/day13-testInput.txt"
      let trackMap = fst $ parseRawInput contents
      let cart = Cart (Coordinate 0 1) North L
      (moveCart cart trackMap) `shouldBe` Cart (Coordinate 0 0) East L
    it "should move the cart in the direction it is facing and turn left on the backSlash" $ do
      contents <- readFile "test/resources/day13-testInput.txt"
      let trackMap = fst $ parseRawInput contents
      let cart = Cart (Coordinate 4 1) North L
      (moveCart cart trackMap) `shouldBe` Cart (Coordinate 4 0) West L
    it "should move the cart in the direction it is facing and turn left on the crossing, next turn should be FW" $ do
      contents <- readFile "test/resources/day13-testInput.txt"
      let trackMap = fst $ parseRawInput contents
      let cart = Cart (Coordinate 4 1) South L
      (moveCart cart trackMap) `shouldBe` Cart (Coordinate 4 2) East FW
    it "should move the cart in the direction it is facing and turn right on the crossing, next turn should be L" $ do
      contents <- readFile "test/resources/day13-testInput.txt"
      let trackMap = fst $ parseRawInput contents
      let cart = Cart (Coordinate 4 1) South R
      (moveCart cart trackMap) `shouldBe` Cart (Coordinate 4 2) West L


