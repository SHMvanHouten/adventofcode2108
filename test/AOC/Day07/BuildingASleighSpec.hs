module AOC.Day07.BuildingASleighSpec (spec, main) where

import Test.Hspec
import Test.QuickCheck

import AOC.Day07.BuildingASleigh

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "it should order the steps after the instructions" $ do
    it "order test input steps" $ do
      getInstructionOrder testInput `shouldBe` "CABDFE"
  describe "parse raw file" $ do
    it "it should parse the instructions" $ do
      rawInstructionsToInstructions testInput `shouldContain` [Instructions 'C' 'A']

loadFile :: IO String
loadFile = do
  readFile "inputday7.txt"

testInput = "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin."