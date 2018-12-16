module AOC.Day15.GoblinSlayingSpec where

import Test.Hspec
import Test.QuickCheck

import AOC.Day15.GoblinSlaying
import AOC.Util.Coordinate
import qualified Data.Set as Set
import qualified Data.Map as Map


main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "it should solve the challenge input" $ do
    it "should solve the challenge pt 2" $ do
      pending

  describe "parseBattleCave" $ do
    it "parses the raw input into a battle cave" $ do
      let simpleInput = "#.E.G.\n" ++
                        "#.G.E."
      let expectedWalls = Set.fromList [Coordinate 0 0, Coordinate 0 1]
      let expectedElves = Map.fromList [(Coordinate 2 0, Npc (Coordinate 2 0) 200), (Coordinate 4 1, Npc (Coordinate 4 1) 200)]
      let expectedGoblins = Map.fromList [(Coordinate 4 0, Npc (Coordinate 4 0) 200), (Coordinate 2 1, Npc (Coordinate 2 1) 200)]
      parseBattleCave simpleInput `shouldBe` BattleCave expectedWalls expectedElves expectedGoblins

testInput = "#######\n"++
            "#.G...#\n"++
            "#...EG#\n"++
            "#.#.#G#\n"++
            "#..G#E#\n"++
            "#.....#\n"++
            "#######\n"