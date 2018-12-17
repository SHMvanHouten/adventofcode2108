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

  describe "it should solve the challenge part 2" $ do
    it "solves the challenge" $ do
      content <- readFile "resources/input-day15.txt"
      findFirstTimeAllElvesSurvive content 30 `shouldBe` (40625,34)

  describe "it should solve the inputs" $ do
    it "should solve the test input" $ do
      let initialBattleCave = parseBattleCave testInput 3
      let result = resolveCaveConflict initialBattleCave 0
      print $ fst result
      snd result `shouldBe` 27730
    it "should solve the other test input" $ do
      let initialBattleCave = parseBattleCave otherTestInput 3
      let result = resolveCaveConflict initialBattleCave 0
      print $ fst result
      snd result `shouldBe` 36334
    it "should solve the other test input" $ do
      let input = "#######\n"++
                  "#E..EG#\n"++
                  "#.#G.E#\n"++
                  "#E.##E#\n"++
                  "#G..#.#\n"++
                  "#..E#.#\n"++
                  "#######\n"
      let initialBattleCave = parseBattleCave input 3
      let result = resolveCaveConflict initialBattleCave 0
      print $ fst result
      snd result `shouldBe` 39514
    it "should solve the other test input" $ do
          let input = "#######\n"++
                      "#E.G#.#\n"++
                      "#.#G..#\n"++
                      "#G.#.G#\n"++
                      "#G..#.#\n"++
                      "#...E.#\n"++
                      "#######\n"
          let initialBattleCave = parseBattleCave input 3
          let result = resolveCaveConflict initialBattleCave 0
          print $ fst result
          snd result `shouldBe` 27755
    it "should solve the other test input" $ do
          let input = "#######\n"++
                      "#.E...#\n"++
                      "#.#..G#\n"++
                      "#.###.#\n"++
                      "#E#G#G#\n"++
                      "#...#G#\n"++
                      "#######\n"
          let initialBattleCave = parseBattleCave input 3
          let result = resolveCaveConflict initialBattleCave 0
          print $ fst result
          snd result `shouldBe` 28944
    it "should solve the other test input" $ do
          let input = "#########\n"++
                      "#G......#\n"++
                      "#.E.#...#\n"++
                      "#..##..G#\n"++
                      "#...##..#\n"++
                      "#...#...#\n"++
                      "#.G...G.#\n"++
                      "#.....G.#\n"++
                      "#########\n"
          let initialBattleCave = parseBattleCave input 3
          let result = resolveCaveConflict initialBattleCave 0
          print $ fst result
          snd result `shouldBe` 18740

    it "should solve a simple case" $ do
      let input = "#######\n"++
                  "#G....#\n"++
                  "#.G...#\n"++
                  "#.#.#G#\n"++
                  "#...#E#\n"++
                  "#....G#\n"++
                  "#######\n"
      let battleCave = parseBattleCave input 3
      let updateElves = Map.fromList [(Coordinate 5 4, (Npc (Coordinate 5 4) 7 Elves 3))]
      let caveWithWeakElf = BattleCave (walls battleCave) updateElves (goblins battleCave)
      let result = resolveCaveConflict caveWithWeakElf 0
      snd result `shouldBe` 797

  describe "parseBattleCave" $ do
    it "parses the raw input into a battle cave" $ do
      let simpleInput = "#.E.G.\n" ++
                        "#.G.E."
      let expectedWalls = Set.fromList [Coordinate 0 0, Coordinate 0 1]
      let expectedElves = Map.fromList [(Coordinate 2 0, Npc (Coordinate 2 0) 200 Elves 3), (Coordinate 4 1, Npc (Coordinate 4 1) 200 Elves 3)]
      let expectedGoblins = Map.fromList [(Coordinate 4 0, Npc (Coordinate 4 0) 200 Goblins 3), (Coordinate 2 1, Npc (Coordinate 2 1) 200 Goblins 3)]
      parseBattleCave simpleInput 3 `shouldBe` BattleCave expectedWalls expectedElves expectedGoblins

  describe "doMove" $ do
    it "the elf should move a step towards the goblin" $ do
      let simpleInput = "#.E..G.#"
      let elf = Npc (Coordinate 2 0) 200 Elves 3
      let simpleBattleCave = parseBattleCave simpleInput 3
      moveNpc elf simpleBattleCave `shouldBe` Npc (Coordinate 3 0) 200 Elves 3

-- todo : fails now because damage
--  describe "move all npcs" $ do
--    it "moves all the npcs one step" $ do
--      let input = "#########\n"++
--                  "#G..G..G#\n"++
--                  "#.......#\n"++
--                  "#.......#\n"++
--                  "#G..E..G#\n"++
--                  "#.......#\n"++
--                  "#.......#\n"++
--                  "#G..G..G#\n"++
--                  "#########\n"
--      let expectedOutput ="#########\n"++
--                          "#.G...G.#\n"++
--                          "#...G...#\n"++
--                          "#...E..G#\n"++
--                          "#.G.....#\n"++
--                          "#.......#\n"++
--                          "#G..G..G#\n"++
--                          "#.......#\n"++
--                          "#########\n"
--      let startingCave = parseBattleCave input
--      let expectedCave = parseBattleCave expectedOutput
--      doTurn startingCave `shouldBe` expectedCave

  describe "attack and move npcs" $ do
    it "should attack one elf and goblin and move the rest" $ do
      let expectedOutput = parseBattleCave testInput2 3
      let expectedElves = Map.fromList [(Coordinate 4 2, Npc (Coordinate 4 2) 197 Elves 3), (Coordinate 5 4, Npc (Coordinate 5 4) 197 Elves 3)]
      let expectedGoblins = Map.insert (Coordinate 5 3 ) (Npc (Coordinate 5 3) 197 Goblins 3) $ Map.insert (Coordinate 5 2) (Npc (Coordinate 5 2) 197 Goblins 3) (goblins expectedOutput)
      let expectedBattleCave = BattleCave (walls expectedOutput) expectedElves expectedGoblins
      doTurn (parseBattleCave testInput 3) `shouldBe` expectedBattleCave
    it "should kill the last elf" $ do
      let input = "#######\n"++
                  "#G....#\n"++
                  "#.G...#\n"++
                  "#.#.#G#\n"++
                  "#...#E#\n"++
                  "#....G#\n"++
                  "#######\n"
      let battleCave = parseBattleCave input 3
      let updateElves = Map.fromList [(Coordinate 5 4, (Npc (Coordinate 5 4) 3 Elves 3))]
      let caveWithWeakElf = BattleCave (walls battleCave) updateElves (goblins battleCave)
      let result = doTurn caveWithWeakElf
      (elves result) `shouldBe` Map.empty

-------------0123456
testInput = "#######\n"++
            "#.G...#\n"++
            "#...EG#\n"++
            "#.#.#G#\n"++
            "#..G#E#\n"++
            "#.....#\n"++
            "#######\n"

testInput2 = "#######\n"++
             "#..G..#\n"++
             "#...EG#\n"++
             "#.#G#G#\n"++
             "#...#E#\n"++
             "#.....#\n"++
             "#######\n"
------------------0123456
otherTestInput = "#######\n"++
                 "#G..#E#\n"++
                 "#E#E.E#\n"++
                 "#G.##.#\n"++
                 "#...#E#\n"++
                 "#...E.#\n"++
                 "#######\n"
