module AOC.Day07.BuildingASleighSpec (spec, main) where

import Test.Hspec
import Test.QuickCheck

import AOC.Day07.BuildingASleigh
import qualified Data.Map as Map

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  -------------------------------
  -- PART 1
  -------------------------------
  describe "it should order the steps after the instructions" $ do
    it "should solve the part 1 test input" $ do
      getInstructionOrder testInput `shouldBe` "CABDFE"

  describe "parse raw file" $ do
    it "it should parse the instructions" $ do
      rawInstructionsToInstructions testInput `shouldContain` [Instruction 'C' 'A']

  describe "build lookup table" $ do
    it "should make a lookup table showing dependencies of the steps" $ do
      let input = [Instruction 'A' 'B']
      (StepOrder 'B' ['A'] []) `shouldBe` ((toStepLookupTable input Map.empty)Map.!'B')

  describe "A and F should be afters of C" $ do
    it "should make a lookup table where C is a before of A, while B and D are afters" $ do
      let lookupTable = toStepLookupTable (rawInstructionsToInstructions testInput) Map.empty
      (StepOrder 'A' ['C'] ['D', 'B']) `shouldBe` lookupTable Map.! 'A'

  describe "get first available step" $ do
    it "should identify C as the first available step" $ do
      let lookupTable = toStepLookupTable (rawInstructionsToInstructions testInput) Map.empty
      getFirstAvailableSteps lookupTable `shouldBe` "C"

  describe "is not dependent on unclaimed step" $ do
    it "should say A is not dependent on an unclaimed step" $ do
      let stepOrder = StepOrder 'A' ['C'] []
      isNotDependentOnUnfinishedStep stepOrder "CJB" `shouldBe` True

testInput = "Step C must be finished before step A can begin.\n" ++
  "Step C must be finished before step F can begin.\n" ++
  "Step A must be finished before step B can begin.\n" ++
  "Step A must be finished before step D can begin.\n" ++
  "Step B must be finished before step E can begin.\n" ++
  "Step D must be finished before step E can begin.\n" ++
  "Step F must be finished before step E can begin."