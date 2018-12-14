module AOC.Day12.PlantSpreadingSpec where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as Map
import AOC.Day12.PlantSpreading

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
-- todo: make a dictionary with solved problems and their solutions
  describe "it should solve the challenge input" $ do
    it "should solve the challenge" $ do
      contents <- readFile "resources/input-day12.txt"
      let input = parseRawPlantInstructions contents
      let plantsToIndex = passGenerations 20 (fst input) ((snd input), 0)
      calculateTotalPlantValue (fst plantsToIndex) (snd plantsToIndex) `shouldBe` 2040

  describe "find the value after 20 generations of the plants " $ do
    it "runs through 20 generations of plants and gets the value of it" $ do
      let input = parseRawPlantInstructions testInput
      let plantsToIndex = passGenerations 20 (fst input) ((snd input), 0)
      calculateTotalPlantValue (fst plantsToIndex) (snd plantsToIndex) `shouldBe` 325

  describe "find the next generation value of the plants " $ do
    it "gets the next value generation" $ do
        pending

  describe "parse" $ do
    it "turns the input into a pair of the plantrow and a Map of instructions" $ do
      let input = "initial state: #.#\n" ++
                  "\n"++
                  "..#.. => #\n" ++
                  "..### => .\n"
      parseRawPlantInstructions input `shouldBe` ((Map.fromList [("..#..", '#'), ("..###", '.')]), "#.#")

  describe "passGenerations" $ do
    it "should calculate what the state of the plants is after one generation has passed" $ do
      let input = parseRawPlantInstructions testInput
      let plantsToIndex = passGenerations 1 (fst input) ((snd input), 0)
      (fst plantsToIndex) `shouldBe` "#...#....#.....#..#..#..#"
      (snd plantsToIndex) `shouldBe` 0
    it "should calculate what the state of the plants is after two generations have passed" $ do
      let input = parseRawPlantInstructions testInput
      let plantsToIndex = passGenerations 2 (fst input) ((snd input), 0)
      (fst plantsToIndex) `shouldBe` "##..##...##....#..#..#..##"
      (snd plantsToIndex) `shouldBe` 0
    it "should calculate what the state of the plants is after three generations have passed" $ do
      let input = parseRawPlantInstructions testInput
      let plantsToIndex = passGenerations 3 (fst input) ((snd input), 0)
      (fst plantsToIndex) `shouldBe` "#.#...#..#.#....#..#..#...#"
      (snd plantsToIndex) `shouldBe` -1
    it "should calculate what the state of the plants is after four generations have passed" $ do
      let input = parseRawPlantInstructions testInput
      let plantsToIndex = passGenerations 4 (fst input) ((snd input), 0)
      (fst plantsToIndex) `shouldBe` "#.#..#...#.#...#..#..##..##"
      (snd plantsToIndex) `shouldBe` 0
    it "should calculate what the state of the plants is after twenty generations have passed" $ do
      let input = parseRawPlantInstructions testInput
      let plantsToIndex = passGenerations 20 (fst input) ((snd input), 0)
      (fst plantsToIndex) `shouldBe` "#....##....#####...#######....#.#..##"
      (snd plantsToIndex) `shouldBe` -2

  describe "procreate end" $ do
    it "end case: should trim the edges and reverse" $ do
      let instructions = fst $ parseRawPlantInstructions testInput
      procreate "#..."  instructions 0 ".##########.."`shouldBe` ("##########", 2)

  describe "getCenterValue" $ do
    it "gives the result of the 5 pots procreating" $ do
      let instructions = fst $ parseRawPlantInstructions testInput
      getCenterValue "...##" instructions `shouldBe` '#'

  describe "trimPotsAtEgde" $ do
    it "trims the left edge of empty pots" $ do
      trimEmptyPotsAtEdge ".....######" `shouldBe` "######"

testInput = "initial state: #..#.#..##......###...###\n"++
            "\n"++
            "..### => .\n"++
            "..... => .\n"++
            "..#.. => #\n"++
            ".###. => .\n"++
            "...## => #\n"++
            "#.### => #\n"++
            "#.#.# => #\n"++
            "##..# => .\n"++
            "##.## => #\n"++
            "#...# => .\n"++
            "..##. => .\n"++
            "##.#. => #\n"++
            "...#. => .\n"++
            "#..#. => .\n"++
            ".#### => #\n"++
            ".#..# => .\n"++
            "##... => .\n"++
            ".##.# => .\n"++
            "....# => .\n"++
            "#.... => .\n"++
            ".#.#. => #\n"++
            ".##.. => #\n"++
            "###.# => #\n"++
            "####. => #\n"++
            "##### => .\n"++
            "#.##. => .\n"++
            ".#... => #\n"++
            ".#.## => #\n"++
            "###.. => #\n"++
            "#..## => .\n"++
            "#.#.. => .\n"++
            "..#.# => .\n"
