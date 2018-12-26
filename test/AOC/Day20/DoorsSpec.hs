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
      pending

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
