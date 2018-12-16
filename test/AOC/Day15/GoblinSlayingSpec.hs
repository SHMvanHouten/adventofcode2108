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
    it "should solve the challenge" $ do
      pending

  describe "parseBattleCave" $ do
    it "parses the raw input into a battle cave" $ do
      let simpleInput = "#.E.G.\n" ++
                        "#.G.E."
      let expectedWalls = Set.fromList [Coordinate 0 0, Coordinate 0 1]
      let expectedElves = Map.fromList [(Coordinate 2 0, Npc (Coordinate 2 0) 200 Elves), (Coordinate 4 1, Npc (Coordinate 4 1) 200 Elves)]
      let expectedGoblins = Map.fromList [(Coordinate 4 0, Npc (Coordinate 4 0) 200 Goblins), (Coordinate 2 1, Npc (Coordinate 2 1) 200 Goblins)]
      parseBattleCave simpleInput `shouldBe` BattleCave expectedWalls expectedElves expectedGoblins

  describe "doMove" $ do
    it "the elf should move a step towards the goblin" $ do
      let simpleInput = "#.E..G.#"
      let elf = Npc (Coordinate 2 0) 200 Elves
      let simpleBattleCave = parseBattleCave simpleInput
      moveNpc elf simpleBattleCave `shouldBe` Npc (Coordinate 3 0) 200 Elves

  describe "move all npcs" $ do
    it "moves all the npcs one step" $ do
      let input = "#########\n"++
                  "#G..G..G#\n"++
                  "#.......#\n"++
                  "#.......#\n"++
                  "#G..E..G#\n"++
                  "#.......#\n"++
                  "#.......#\n"++
                  "#G..G..G#\n"++
                  "#########\n"
      let expectedOutput ="#########\n"++
                          "#.G...G.#\n"++
                          "#...G...#\n"++
                          "#...E..G#\n"++
                          "#.G.....#\n"++
                          "#.......#\n"++
                          "#G..G..G#\n"++
                          "#.......#\n"++
                          "#########\n"
      let startingCave = parseBattleCave input
      let expectedCave = parseBattleCave expectedOutput
      doTurn startingCave `shouldBe` expectedCave


testInput = "#######\n"++
            "#.G...#\n"++
            "#...EG#\n"++
            "#.#.#G#\n"++
            "#..G#E#\n"++
            "#.....#\n"++
            "#######\n"