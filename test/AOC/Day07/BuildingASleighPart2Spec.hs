module AOC.Day07.BuildingASleighPart2Spec (spec, main) where

import Test.Hspec
import Test.QuickCheck

import AOC.Day07.BuildingASleigh
import qualified Data.Map as Map
import Data.List

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  ----------------------
  -- PART 2
  ----------------------

  describe "doing the test input should take 15 seconds" $ do
    it "should take 15 seconds" $ do
      let result = timeTheSleighBuild testInput 0 2
      snd result `shouldBe` "CABFDE"
      fst result  `shouldBe` 15
--
--  describe "simple input should take 8 seconds" $ do
--    it "should take 8 seconds" $ do
--       let result = timeTheSleighBuild simpleInput 0 2
--       snd result `shouldBe` "CABD"
--       fst result `shouldBe` 8

  describe "partition ticked workers" $ do
    it "should split the steps up in finished and unfinished steps" $ do
      let input = [(StepBuild 'A' 1), (StepBuild 'B' 1), (StepBuild 'F' 6), (StepBuild 'C' 2)]
      let expectedResult = ([(StepBuild 'A' 0), (StepBuild 'B' 0)], [(StepBuild 'F' 5), (StepBuild 'C' 1)])
      partition (\x -> secondsLeft x == 0) (map (tickSecond) input) `shouldBe` expectedResult

  describe "assignWorkers" $ do
    it "should assign three steps" $ do
      assignWorkers ['A', 'B', 'F'] 0 `shouldBe` [(StepBuild 'A' 1), (StepBuild 'B' 2), (StepBuild 'F' 6)]

  describe "get build time " $do
    it " should get the build time for the step " $ do
      getBuildTime 'F' 10 `shouldBe` 16

  describe "findAvailableSteps" $do
    it "should find steps F when C is finished and A is unfinished" $ do
      let lookupTable = toStepLookupTable (rawInstructionsToInstructions testInput) Map.empty
      let finishedSteps = ['C']
      let unfinishedSteps = ['A']
      findAvailableSteps finishedSteps lookupTable unfinishedSteps `shouldBe` ['F']

  describe "assign workers to new steps" $ do
    it " should assign workers to the steps" $ do
      let existingWorkers = [(StepBuild 'A' 5), (StepBuild 'B' 6)]
      let expectedAssignedWorkers = [(StepBuild 'A' 5), (StepBuild 'B' 6), (StepBuild 'C' 3), (StepBuild 'D' 4)]
      assignWorkersToNewSteps existingWorkers 4 ['C', 'D', 'E', 'F'] 0 `shouldBe` expectedAssignedWorkers

testInput = "Step C must be finished before step A can begin.\n" ++
  "Step C must be finished before step F can begin.\n" ++
  "Step A must be finished before step B can begin.\n" ++
  "Step A must be finished before step D can begin.\n" ++
  "Step B must be finished before step E can begin.\n" ++
  "Step D must be finished before step E can begin.\n" ++
  "Step F must be finished before step E can begin."

simpleInput = "Step C must be finished before step A can begin.\n" ++
  "Step A must be finished before step B can begin.\n" ++
  "Step A must be finished before step D can begin.\n"