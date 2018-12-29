module AOC.Day23.NanoBotsSpec where

import Test.Hspec
import Test.QuickCheck
import AOC.Day23.NanoBots
import AOC.Util.Coord3D
main :: IO ()
main = hspec spec

spec :: Spec
spec = do

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
