module AOC.Day18.ForestrySpec where

import Test.Hspec
import Test.QuickCheck

import AOC.Day18.Forestry
import AOC.Util.Coordinate

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "solves the challenge input" $ do
      it "solves the challenge input" $ do
        challengeInput <- readFile "resources/input-day18.txt"
        let acres = parseInput challengeInput
        let result = evolveMinutes 10 acres
        treesTimesLumberYards result `shouldBe` 745008

  describe "solves the challenge input part 2" $ do
    it "solves the challenge input part 2 " $ do
      challengeInput <- readFile "resources/input-day18.txt"
      let acres = parseInput challengeInput
      let result = evolveMinutes 10 acres
      treesTimesLumberYards result `shouldBe` 745008

  describe "solves the test input" $ do
      it "evolves the test input ten minutes" $ do
        let testInput =      ".#.#...|#.\n"++
                             ".....#|##|\n"++
                             ".|..|...#.\n"++
                             "..|#.....#\n"++
                             "#.#|||#|#|\n"++
                             "...#.||...\n"++
                             ".|....|...\n"++
                             "||...#|.#|\n"++
                             "|.||||..|.\n"++
                             "...#.|..|.\n"
        let acres = parseInput testInput
        let expectedOutput = ".||##.....\n"++
                             "||###.....\n"++
                             "||##......\n"++
                             "|##.....##\n"++
                             "|##.....##\n"++
                             "|##....##|\n"++
                             "||##.####|\n"++
                             "||#####|||\n"++
                             "||||#|||||\n"++
                             "||||||||||\n"
        let result = evolveMinutes 10 acres
        drawLumberMap result `shouldBe` expectedOutput
        print $ getAcreByAcreType result
        treesTimesLumberYards result `shouldBe` 1147

  describe "evolves test input one minute" $ do
    it "evolves the test input one minute" $ do
      let testInput =      ".#.#...|#.\n"++
                           ".....#|##|\n"++
                           ".|..|...#.\n"++
                           "..|#.....#\n"++
                           "#.#|||#|#|\n"++
                           "...#.||...\n"++
                           ".|....|...\n"++
                           "||...#|.#|\n"++
                           "|.||||..|.\n"++
                           "...#.|..|.\n"
      let acres = parseInput testInput
      let expectedOutput = ".......##.\n"++
                           "......|###\n"++
                           ".|..|...#.\n"++
                           "..|#||...#\n"++
                           "..##||.|#|\n"++
                           "...#||||..\n"++
                           "||...|||..\n"++
                           "|||||.||.|\n"++
                           "||||||||||\n"++
                           "....||..|.\n"
      drawLumberMap (evolveMinute acres) `shouldBe` expectedOutput

  describe "evolveAcre" $ do
    it " an open acre becomes filled with trees if three or more adjacent acres contained trees" $ do
      let surroundingAcresRaw = ".|.\n" ++
                                "|.|\n" ++
                                ".#.\n"
      let surroundingAcres = parseInput surroundingAcresRaw
      evolveAcre (Acre (Coordinate 1 1) Open) surroundingAcres `shouldBe` (Acre (Coordinate 1 1) Tree)
    it " an open acre stays open if less than three adjacent acres contained trees" $ do
      let surroundingAcresRaw = ".#.\n" ++
                                "|.|\n" ++
                                ".#.\n"
      let surroundingAcres = parseInput surroundingAcresRaw
      evolveAcre (Acre (Coordinate 1 1) Open) surroundingAcres `shouldBe` (Acre (Coordinate 1 1) Open)
    it " a tree acre becomes a lumberyard if three or more adjacent acres contained lumberyards" $ do
      let surroundingAcresRaw = "##.\n" ++
                                ".||\n" ++
                                ".#.\n"
      let surroundingAcres = parseInput surroundingAcresRaw
      evolveAcre (Acre (Coordinate 1 1) Tree) surroundingAcres `shouldBe` (Acre (Coordinate 1 1) Lumberyard)
    it " a tree acre stays a tree if les than three adjacent acres were lumberyards" $ do
      let surroundingAcresRaw = ".#.\n" ++
                                "#||\n" ++
                                "...\n"
      let surroundingAcres = parseInput surroundingAcresRaw
      evolveAcre (Acre (Coordinate 1 1) Tree) surroundingAcres `shouldBe` (Acre (Coordinate 1 1) Tree)
    it " a lumberyard acre becomes open if it was not adjacant to both a lumberyard and a tree" $ do
      let surroundingAcresRaw = ".#.\n" ++
                                ".#.\n" ++
                                "...\n"
      let surroundingAcres = parseInput surroundingAcresRaw
      evolveAcre (Acre (Coordinate 1 1) Lumberyard) surroundingAcres `shouldBe` (Acre (Coordinate 1 1) Open)
    it " a lumberyard acre remains a lumberyard if it was adjacant to both a lumberyard and a tree" $ do
      let surroundingAcresRaw = ".|.\n" ++
                                ".##\n" ++
                                ".#.\n"
      let surroundingAcres = parseInput surroundingAcresRaw
      evolveAcre (Acre (Coordinate 1 1) Lumberyard) surroundingAcres `shouldBe` (Acre (Coordinate 1 1) Lumberyard)

  describe "edgeAcre" $ do
    it "should not count the nonexistant acre to the left" $ do
      let surroundingAcresRaw = "#.\n" ++
                                "#.\n" ++
                                "..\n"
      let surroundingAcres = parseInput surroundingAcresRaw
      evolveAcre (Acre (Coordinate 0 1) Lumberyard) surroundingAcres `shouldBe` (Acre (Coordinate 0 1) Open)

  describe "parse and draw" $ do
    it "should parse and draw the test input" $ do
      let testInput = ".#.#...|#.\n"++
                      ".....#|##|\n"++
                      ".|..|...#.\n"++
                      "..|#.....#\n"++
                      "#.#|||#|#|\n"++
                      "...#.||...\n"++
                      ".|....|...\n"++
                      "||...#|.#|\n"++
                      "|.||||..|.\n"++
                      "...#.|..|.\n"
      let acres = parseInput testInput
      let drawnAcres = drawLumberMap acres
      drawnAcres `shouldBe` testInput