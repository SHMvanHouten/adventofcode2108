module AOC.Day23.NanoBotsSpec where

import Test.Hspec
import Test.QuickCheck
import AOC.Day23.NanoBots
import AOC.Util.Coord3D
import qualified Data.Sequence as Seq
main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "testInput part 2" $ do
      it "finds the point that is in the range of most bots" $ do
        let input = "pos=<10,12,12>, r=2\n"++
                    "pos=<12,14,12>, r=2\n"++
                    "pos=<16,12,12>, r=4\n"++
                    "pos=<14,14,14>, r=6\n"++
                    "pos=<50,50,50>, r=200\n"++
                    "pos=<10,10,10>, r=5\n"
        let bots = parseInput input
        findPointInRangeOfMostBots bots `shouldBe` Coord3d 12 12 12
  describe "challenge part 1" $ do
    it "solves the challenge input" $ do
      input <- readFile "resources/input-day23.txt"
      let bots = parseInput input
      let bigBot = maximum bots
      getAmountOfBotsInRadius bigBot bots `shouldBe` 588
  describe "testInput" $ do
    it "finds the highest nano bot and finds how many bots are in range" $ do
      let input = "pos=<1,0,0>, r=1\n"++
                  "pos=<4,0,0>, r=3\n"++
                  "pos=<0,0,0>, r=4\n"++
                  "pos=<0,2,0>, r=1\n"++
                  "pos=<0,5,0>, r=3\n"++
                  "pos=<0,0,3>, r=1\n"++
                  "pos=<1,1,1>, r=1\n"++
                  "pos=<1,1,2>, r=1\n"++
                  "pos=<1,3,1>, r=1\n"
      let bots = parseInput input
      let bigBot = maximum bots
--      putStrLn $ unlines $ map (show) bots
      bigBot `shouldBe` NanoBot (Coord3d 0 0 0) 4
      getAmountOfBotsInRadius bigBot bots `shouldBe` 7
