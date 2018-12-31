module AOC.Day25.ConstellationsSpec where

import Test.Hspec
import Test.QuickCheck

import AOC.Day25.Constellations
import AOC.Util.Coord4D

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "solves the challenge input" $ do
    it "finds the amount of constellations for the challenge input" $ do
      input <- readFile "resources/input-day25.txt"
      let coords = parseInput input
      let constellations = assembleConstellations coords []
      length constellations `shouldBe` 350
      (sum $ map (length) constellations) `shouldBe` length coords

  describe "solves the test input" $ do
    it "finds the amount of constellations for test input" $ do
      let coords = parseInput $ "0,0,0,0\n"++
                                "3,0,0,0\n"++
                                "0,3,0,0\n"++
                                "0,0,3,0\n"++
                                "0,0,0,3\n"++
                                "0,0,0,6\n"++
                                "9,0,0,0\n"++
                                "12,0,0,0\n"
      let constellations = assembleConstellations coords []
      length constellations `shouldBe` 2
    it "finds the amount of constellations for test input" $ do
      let coords = parseInput $ "0,0,0,0\n"++
                                "3,0,0,0\n"++
                                "0,3,0,0\n"++
                                "0,0,3,0\n"++
                                "0,0,0,3\n"++
                                "6,0,0,0\n"++
                                "9,0,0,0\n"++
                                "12,0,0,0\n"
      let constellations = assembleConstellations coords []
      length constellations `shouldBe` 1
      length (constellations!!0) `shouldBe` 8

    it "finds the amount of constellations for test input" $ do
      let coords = parseInput testInput1
      let constellations = assembleConstellations coords []
      length constellations `shouldBe` 4
    it "finds the amount of constellations for test input" $ do
      let coords = parseInput $ "1,-1,0,1\n"++
                                "2,0,-1,0\n"++
                                "3,2,-1,0\n"++
                                "0,0,3,1\n"++
                                "0,0,-1,-1\n"++
                                "2,3,-2,0\n"++
                                "-2,2,0,0\n"++
                                "2,-2,0,-1\n"++
                                "1,-1,0,-1\n"++
                                "3,2,0,2\n"
      let constellations = assembleConstellations coords []
      length constellations `shouldBe` 3
    it "finds the amount of constellations for test input" $ do
      let coords = parseInput $ "1,-1,-1,-2\n"++
                                "-2,-2,0,1\n"++
                                "0,2,1,3\n"++
                                "-2,3,-2,1\n"++
                                "0,2,3,-2\n"++
                                "-1,-1,1,-2\n"++
                                "0,-2,-1,0\n"++
                                "-2,2,3,-1\n"++
                                "1,2,2,0\n"++
                                "-1,-2,0,-2\n"
      let constellations = assembleConstellations coords []
      length constellations `shouldBe` 8


testInput1 = "-1,2,2,0\n"++
            "0,0,2,-2\n"++
            "0,0,0,-2\n"++
            "-1,2,0,0\n"++
            "-2,-2,-2,2\n"++
            "3,0,2,-1\n"++
            "-1,3,2,2\n"++
            "-1,0,-1,0\n"++
            "0,2,1,-2\n"++
            "3,0,0,0\n"