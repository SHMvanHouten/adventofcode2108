module AOC.Day07.BuildingASleigh where

import qualified Data.Map as Map
import Data.List
import Data.Char

----------------------
-- PART 2
----------------------
allsteps = ['A'..'Z']

timeTheSleighBuild :: String -> Int -> Int -> (Int, [Step])
timeTheSleighBuild rawInstructions baseStepBuildTime availableWorkers = do
  let lookupTable = buildLookupTableFromRawInstructions rawInstructions
  calculateTimeToExecuteTheBuild lookupTable baseStepBuildTime (getFirstAvailableSteps lookupTable) availableWorkers

calculateTimeToExecuteTheBuild :: StepLookupTable -> Int -> [Step] -> Int -> (Int, [Step])
calculateTimeToExecuteTheBuild lookupTable baseBuildTime availableSteps availableWorkers = do
  let assignedSteps = assignWorkers (take availableWorkers (sort availableSteps)) baseBuildTime
  tickSeconds lookupTable assignedSteps availableWorkers 0 [] baseBuildTime

tickSeconds :: StepLookupTable -> [StepBuild] -> Int -> Int -> [Step] -> Int -> (Int, [Step])
tickSeconds _ [] _ secondsTicked finishedSteps _ = (secondsTicked, finishedSteps)
tickSeconds lookupTable assignedSteps availableWorkers secondsTicked finishedSteps baseBuildTime = do
  let finishedToUnfinishedSteps = partition (\x -> secondsLeft x == 0) (map (tickSecond) assignedSteps)
  let newFinishedSteps = finishedSteps ++ (map (currentStep) (fst finishedToUnfinishedSteps))
  let unfinishedSteps = snd finishedToUnfinishedSteps
  let newAvailableSteps = findAvailableSteps newFinishedSteps lookupTable (map (currentStep) unfinishedSteps)
  let newAssignedSteps = assignWorkersToNewSteps unfinishedSteps availableWorkers newAvailableSteps baseBuildTime
  tickSeconds lookupTable newAssignedSteps availableWorkers (secondsTicked + 1) newFinishedSteps baseBuildTime

assignWorkersToNewSteps :: [StepBuild] -> Int -> [Step] -> Int -> [StepBuild]
assignWorkersToNewSteps unfinishedSteps totalWorkersAvailable availableSteps baseBuildTime = do
  let availableWorkers = totalWorkersAvailable - (length unfinishedSteps)
  unfinishedSteps ++ (assignWorkers (take availableWorkers availableSteps) baseBuildTime)

findAvailableSteps :: [Step] -> StepLookupTable -> [Step] -> [Step]
findAvailableSteps finishedSteps stepLookupTable unfinishedSteps = sort ((filter (\x -> isNotDependentOnUnfinishedStep (stepLookupTable Map.! x) finishedSteps) allsteps) \\ (finishedSteps ++ unfinishedSteps))

tickSecond stepBuild = StepBuild (currentStep stepBuild) (secondsLeft stepBuild - 1)

assignWorkers :: [Step] -> Int ->  [StepBuild]
assignWorkers steps baseBuildTime = map (\x -> toStepBuild x baseBuildTime) steps

toStepBuild :: Step -> Int -> StepBuild
toStepBuild step baseBuildTime = StepBuild step (getBuildTime step baseBuildTime)

getBuildTime step baseBuildTime = baseBuildTime + (ord step) - (ord 'A') + 1

data StepBuild = StepBuild {
  currentStep :: Step,
  secondsLeft :: Int
} deriving (Show, Eq)
----------------------
-- PART 1
----------------------

getInstructionOrder :: String -> String
getInstructionOrder rawInstructions = do
  let lookupTable = buildLookupTableFromRawInstructions rawInstructions
  getStepOrderFromLookupTable lookupTable (getFirstAvailableSteps lookupTable) []

getStepOrderFromLookupTable :: StepLookupTable -> [Step] -> String -> String
getStepOrderFromLookupTable _ [] ordersResult = ordersResult
getStepOrderFromLookupTable stepLookupTable availableSteps orderSoFar = do
  let sortedSteps = sort availableSteps
  let nextStep = head sortedSteps
  let newOrder = orderSoFar ++ [nextStep]
  let restOfSteps = tail sortedSteps
  let newAfters = filter (\x -> isNotDependentOnUnfinishedStep (stepLookupTable Map.! x) newOrder) (afters (stepLookupTable Map.! nextStep))
  let newAvailableSteps = nub ((newAfters ++ restOfSteps ) \\ orderSoFar)
  getStepOrderFromLookupTable stepLookupTable newAvailableSteps newOrder

isNotDependentOnUnfinishedStep :: StepOrder -> String -> Bool
isNotDependentOnUnfinishedStep stepOrder finishedSteps = Data.List.all (\c -> c `elem` finishedSteps) (befores stepOrder)

getFirstAvailableSteps :: StepLookupTable -> [Step]
getFirstAvailableSteps toStepLookupTable = map (step) (Map.elems (Map.filter (\so -> (length (befores so)) == 0) toStepLookupTable))

----------------------
-- Build step lookup table
----------------------

buildLookupTableFromRawInstructions :: String -> StepLookupTable
buildLookupTableFromRawInstructions rawInstructions = do
  let instructions = rawInstructionsToInstructions rawInstructions
  toStepLookupTable instructions Map.empty

toStepLookupTable :: [Instruction] -> StepLookupTable -> StepLookupTable
toStepLookupTable [x] stepLookupTable = addAfterRelation x (addBeforeRelation x stepLookupTable)

toStepLookupTable (x:xs) stepLookupTable= do
  let tableWithRelationsAdded = addAfterRelation x (addBeforeRelation x stepLookupTable)
  toStepLookupTable xs tableWithRelationsAdded

addBeforeRelation :: Instruction -> StepLookupTable -> StepLookupTable
addBeforeRelation instruction stepLookupTable = do
  let afterStep = after instruction
  let beforeStep = before instruction
  Map.insertWith (mergeStepOrder) afterStep (StepOrder afterStep [beforeStep] []) stepLookupTable

addAfterRelation :: Instruction -> StepLookupTable -> StepLookupTable
addAfterRelation instruction stepLookupTable = do
  let afterStep = after instruction
  let beforeStep = before instruction
  Map.insertWith (mergeStepOrder) beforeStep (StepOrder beforeStep [] [afterStep]) stepLookupTable

mergeStepOrder stepOrder1 stepOrder2 = do
  let combinedBefores = (befores stepOrder1) ++ (befores stepOrder2)
  let combinedAfters = (afters stepOrder1) ++ (afters stepOrder2)
  StepOrder (step stepOrder1) combinedBefores combinedAfters

rawInstructionsToInstructions :: String -> [Instruction]
rawInstructionsToInstructions raw = map (toStep) $ lines raw

type StepLookupTable = Map.Map Step StepOrder

data StepOrder = StepOrder {
  step :: Step,
  befores :: [Step],
  afters :: [Step]

} deriving (Show, Eq)

type Step = Char

toStep :: String -> Instruction
toStep rawStep = do
  let split = words rawStep
  Instruction ((split!!1)!!0) ((split!!7)!!0)

data Instruction = Instruction {
  before :: Step,
  after :: Step
} deriving (Show, Eq)