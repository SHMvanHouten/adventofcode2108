module AOC.Day17.ReservoirResearchSpec where

import Test.Hspec
import Test.QuickCheck

import AOC.Day17.ReservoirResearch
import AOC.Util.Coordinate
import qualified Data.Set as Set

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
-- 12160

  describe "challenge part 1" $ do
    it "solves challenge part 1" $ do
--      pending
      content <- readFile "resources/input-day17.txt"
      let clayCoordinates = parseInput content
      let wetSand = locateAllWetSandFaster springLocation clayCoordinates
      printMap clayCoordinates wetSand
      Set.size wetSand `shouldBe` 123

  describe "solve test input" $ do
    it "solves the test input" $ do
      let testInput = "x=495, y=2..7\n"++
                      "y=7, x=495..501\n"++
                      "x=501, y=3..7\n"++
                      "x=498, y=2..4\n"++
                      "x=506, y=1..2\n"++
                      "x=498, y=10..13\n"++
                      "x=504, y=10..13\n"++
                      "y=13, x=498..504\n"
      let clayCoordinates = parseInput testInput
      let wetSand = locateAllWetSandFaster springLocation clayCoordinates
      Set.size wetSand `shouldBe` 57

  describe "the water drops off the map" $ do
    it "fills a single column all the way down to the bottom" $ do
      trickleDown (Coordinate 0 0) Set.empty Set.empty 2 `shouldBe` Set.fromList [(Coordinate 0 0), (Coordinate 0 1), (Coordinate 0 2)]

  describe "the water will hit clay at some point" $ do
    it "trickles down until clay is hit and then goes right and trickles down further" $ do
      --  +
      -- #--
      -- .#|
      -- ..|
      let clayCoords = Set.fromList [(Coordinate 0 1), (Coordinate 1 2)]
      let expectedResult = Set.fromList [(Coordinate 1 0), (Coordinate 1 1), (Coordinate 2 1), (Coordinate 2 2), (Coordinate 2 3)]
      trickleDown (Coordinate 1 0) clayCoords Set.empty 3 `shouldBe` expectedResult

  describe "fill up basin and overflow" $ do
    it "should fill up the basin adn overflow" $ do
      -- ...+....
      -- .#----|.
      -- .#---#|.
      -- .#####|.
      -- ......|.
      let clayCoords = Set.fromList [(Coordinate 1 1), (Coordinate 1 2), (Coordinate 5 2), (Coordinate 1 3), (Coordinate 2 3), (Coordinate 3 3), (Coordinate 4 3), (Coordinate 5 3)]
      let spring = Coordinate 3 0
      trickleDown spring clayCoords Set.empty 5 `shouldBe` Set.fromList [(Coordinate 3 0),(Coordinate 2 1),(Coordinate 3 1),(Coordinate 4 1),(Coordinate 5 1),(Coordinate 6 1),(Coordinate 2 2),(Coordinate 3 2),(Coordinate 4 2),(Coordinate 6 2),(Coordinate 6 3),(Coordinate 6 4),(Coordinate 6 5)]

      pending

  describe "parse input" $ do
    it "should parse the input into a lovely little picture" $ do
      content <- readFile "resources/input-day17.txt"
--      printMap (parseInput content) Set.empty
      pendingWith "if you want a pretty picture you must uncomment above line"

  describe "it should fill the whole basin" $ do
    it "should fill the whole basin" $ do
      let content = "x=1, y=2..12\n"++
                    "x=20, y=2..12\n"++
                    "y=13, x=1..20\n"++
                    "x=7, y=8..10\n"++
                    "x=14, y=8..10\n"++
                    "y=10, x=8..13\n"
      let clayCoordinates = parseInput content
      let wetSand = locateAllWetSand (Coordinate 15 0) clayCoordinates 13
      printMap clayCoordinates wetSand
