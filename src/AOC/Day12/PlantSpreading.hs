module AOC.Day12.PlantSpreading where

import qualified Data.Map as Map

passGenerations :: Integer -> Instructions -> ([Plant], Integer) -> [Plant]
passGenerations amount instructions (plants, startingIndex)
  | amount == 0 = plants
  | otherwise = passGenerations (amount - 1) instructions (procreate plants instructions startingIndex [])

-- to improve performance, we are adding each found new plant
-- to the left of the list
-- and reversing the result at the end (after we trim the empty pots at the end
procreate :: [Plant] -> Instructions -> Integer -> [Plant] -> ([Plant], Integer)

--case: end
procreate (l1:l2:c:r1:[]) instructions startingIndex result = do
  let rightEdge = getCenterValue (l1:l2:c:r1:'.':[]) instructions
  let updatedResult = trimEmptyPotsAtEdge (rightEdge:result)
  let reversedResult = trimEmptyPotsAtEdge $ reverse updatedResult
  (reversedResult, startingIndex)

--case: start
procreate (l2:c:r1:r2:xs) instructions startingIndex []
  | edgeChar == '#' = procreate (c:r1:r2:xs) instructions (startingIndex - 1) "#"
  | otherwise = procreate (c:r1:r2:xs) instructions startingIndex "."
  where edgeChar = getCenterValue ('.':l2:c:r1:r2:[]) instructions

procreate (l1:l2:c:r1:r2:xs) instructions startingIndex result = do
  let updatedResult = (getCenterValue (l1:l2:c:r1:r2:[]) instructions):result
  procreate (l2:c:r1:r2:xs) instructions startingIndex updatedResult

trimEmptyPotsAtEdge :: [Plant] -> [Plant]
trimEmptyPotsAtEdge (x:xs)
  | x == '#' = x:xs
  |otherwise = trimEmptyPotsAtEdge xs

-- calculate left edge and startingindex-- if found plant

getCenterValue :: [Plant] -> Instructions -> Char
getCenterValue fivePots instructions= instructions Map.! fivePots

parseRawPlantInstructions :: String -> (Instructions, [Plant])
parseRawPlantInstructions rawString = (Map.fromList $ map toInstruction (tail $ tail lineSplit), last $ words $head lineSplit)
  where lineSplit = lines rawString

toInstruction :: String -> Instruction
toInstruction rawInstruction = (head split, head $ last split)
  where split = words rawInstruction

type Plant = Char

type Instructions = Map.Map String Char
type Instruction = (String, Char)
--data Plant = Plant {
--  id :: Int,
--  isPlant :: Bool
--} deriving (Show, Eq)
