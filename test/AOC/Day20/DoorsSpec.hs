module AOC.Day20.DoorsSpec where

import Test.Hspec
import Test.QuickCheck

import AOC.Day20.Doors
import Data.Sequence
import AOC.Util.Coordinate

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "challenge" $ do
    it "solves challenge " $ do
      input <- readFile "resources/input-day20.txt"
      let result = parseInput input
      let longestPath = getLongestPath result
      let printedPath = toPrintablePath longestPath
      print printedPath
      Prelude.length printedPath `shouldBe` 4247
    it "finds the amount of longest paths" $ do
      input <- readFile "resources/input-day20.txt"
      let result = parseInput input
      let longPaths = getAllPathsOver1000Long result
      Prelude.length longPaths `shouldBe` 893
    it "solves the challenge part 2" $ do
      input <- readFile "resources/input-day20.txt"
      let result = parseInput input
      let farAwayRooms = getAmountOfRoomsOver1000DoorsAway result
      farAwayRooms `shouldBe` 8356


  describe "test input" $ do
    it " solves the first test input" $ do
      let input = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
      let result = parseInput input
      let longestPath = getLongestPath result
      let printedPath = toPrintablePath longestPath
      printedPath `shouldBe` "ESSWWNNNENNWWWSSSSENNNE"
      Prelude.length printedPath `shouldBe` 23

  it " solves the second test input" $ do
      let input = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
      let result = parseInput input
      let longestPath = getLongestPath result
      let printedPath = toPrintablePath longestPath
      printedPath `shouldBe` "WSSEESWWWNWNENNEEEENNWSWWNWWSSS"
      Prelude.length printedPath `shouldBe` 31

  describe "simple path" $ do
    it "finds the simple path" $ do
      let input = "^WNE$"
      let result = parseInput input
      map (toPrintablePath) result `shouldBe` ["WNE"]

    it "finds two simple routes" $ do
      let input = "^N(E|W)N$"
      let result = parseInput input
      map (toPrintablePath) result `shouldBe` ["NEN", "NWN"]

    it "finds the example routes" $ do
     let input = "^ENWWW(NEEE|SSE(EE|N))$"
     let result = parseInput input
     map (toPrintablePath) result `shouldBe` ["ENWWWNEEE", "ENWWWSSEEE", "ENWWWSSEN"]

    it "finds the example routes" $ do
     let input = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
     let result = parseInput input
     map (toPrintablePath) result `shouldBe` ["ENNWSWWNE", "ENNWSWWSSSEENWN", "ENNWSWWSSSEENEESW", "ENNWSWWSSSEENEENNN"]

  describe "halve" $ do
    it "should halve the string" $ do
      halve "1234" `shouldBe` "12"
      halve "123456" `shouldBe` "123"

