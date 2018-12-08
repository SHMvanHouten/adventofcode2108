module AOC.Day07.BuildingASleigh where

getInstructionOrder :: String -> String
getInstructionOrder rawInstructions = do
  "fail"


rawInstructionsToInstructions :: String -> [Instructions]
rawInstructionsToInstructions raw = map (toStep) $ lines raw

toStep :: String -> Instructions
toStep rawStep = do
  let split = words rawStep
  Instructions ((split!!1)!!0) ((split!!7)!!0)

data Instructions = Instructions {
  before :: Char,
  after :: Char
} deriving (Show, Eq)