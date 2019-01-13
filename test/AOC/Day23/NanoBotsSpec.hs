module AOC.Day23.NanoBotsSpec where

import Test.Hspec
import Test.QuickCheck
import AOC.Day23.NanoBots
import AOC.Util.Coord3D
import qualified Data.Set as Set
import AOC.Day23.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
-- {x = 37628955, y = 23286566, z = 50312122}
-- {x = 40484500, y = 23948900, z = 48118800}
-- {x = 40484603, y = 23948992, z = 48118900}
-- {x = 40484603, y = 23948992, z = 48119001}
-- {x = 37628960, y = 23286570, z = 50312110}
-- 112552200
-- 112552495 too high
-- 113267018 too high Coord3d {x = 39044798, y = 25111110, z = 49111110}

  describe "challenge part  2" $ do
      it "finds the point that is in the range of most bots" $ do
        input <- readFile "resources/input-day23.txt"
        let bots = parseInput input
        challengeFindPointInRangeOfMostBots bots `shouldBe` Coord3d 37628955 23286566 50312122

  describe "challenge part  2" $ do
      it "finds the point that is in the range of most bots" $ do
        input <- readFile "resources/input-day23.txt"
        let bots = parseInput input
        findPointInRangeOfMostBots bots `shouldBe` Coord3d 37628955 23286566 50312122

--  describe "challenge part  2" $ do
--      it "finds the point that is in the range of most bots" $ do
--        input <- readFile "resources/input-day23.txt"
--        let bots = parseInput input
--        let coordRange = getCoordsInRange [40484603..40484603] [23940992..23958992] [48118000..48120000]
--        findPointWithMostBots coordRange bots (stubCoord, 0) `shouldBe` Coord3d 0 0 0

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

  describe "divideBoxInto8" $ do
    it "should divide the even box into equal parts" $ do
      let dividedBoxes = divideBoxInto8 $ Box (Coord3d 0 0 0) (Coord3d 3 3 3)
      length dividedBoxes `shouldBe` 8
      let volumes = map (\(Box (Coord3d minx miny minz) (Coord3d maxx maxy maxz)) -> (maxx - minx) * (maxy - miny) * (maxz - minz)) dividedBoxes
      (length $ filter (== 8) volumes) `shouldBe` 8
    it "should divide the uneven box into equal parts" $ do
        let dividedBoxes = divideBoxInto8 $ Box (Coord3d 0 0 0) (Coord3d 4 4 4)
        length dividedBoxes `shouldBe` 8
        print dividedBoxes
        let volumes = map (\(Box (Coord3d minx miny minz) (Coord3d maxx maxy maxz)) -> (maxx - minx) * (maxy - miny) * (maxz - minz)) dividedBoxes
        (length $ filter (== 8) volumes) `shouldBe` 8

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
