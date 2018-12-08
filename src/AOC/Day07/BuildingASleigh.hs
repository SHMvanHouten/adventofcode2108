module AOC.Day07.BuildingASleigh where

import qualified Data.Map as Map
import Data.List

getInstructionOrder :: String -> String
getInstructionOrder rawInstructions = do
  let instructions = rawInstructionsToInstructions rawInstructions
  let lookupTable = toStepLookupTable instructions Map.empty
  getStepOrderFromLookupTable lookupTable (getFirstAvailableSteps lookupTable) []

getStepOrderFromLookupTable :: StepLookupTable -> [Step] -> String -> String
getStepOrderFromLookupTable _ [] ordersResult = ordersResult
getStepOrderFromLookupTable stepLookupTable availableSteps orderSoFar = do
  let sortedSteps = sort availableSteps
  let nextStep = head sortedSteps
  let restOfSteps = tail sortedSteps
  let newAfters = afters (stepLookupTable Map.! nextStep) \\ concatMap (afters) (getStepOrdersFor restOfSteps stepLookupTable [])
  let newAvailableSteps = nub ((newAfters ++ restOfSteps ) \\ orderSoFar)
  let newOrder = orderSoFar ++ [nextStep]
  getStepOrderFromLookupTable stepLookupTable newAvailableSteps newOrder

getStepOrdersFor :: [Step] -> StepLookupTable -> [StepOrder] -> [StepOrder]
getStepOrdersFor [] _ results = results
getStepOrdersFor (x:xs) stepLookupTable results = (stepLookupTable Map.! x) : results

getFirstAvailableSteps :: StepLookupTable -> [Step]
getFirstAvailableSteps toStepLookupTable = map (step) (Map.elems (Map.filter (\so -> (length (befores so)) == 0) toStepLookupTable))

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