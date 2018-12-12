module AOC.Day12.PlantSpreading where

import qualified Data.Map as Map

calculateTotalPlantValue plants startingIndex = do
              sum $ map (fst) $filter (\x -> snd x == '#') (zip [startingIndex..] plants)

passGenerations :: Int -> Instructions -> ([Plant], Int) -> ([Plant], Int)
passGenerations amount instructions (plants, startingIndex)
  | amount == 0 = (plants, startingIndex)
  | otherwise = passGenerations (amount - 1) instructions (procreate plants instructions startingIndex [])

-- we ignore the pot -2 and +2 from the edges because they will never be plants
-- to improve performance, we are adding each found new plant
-- to the left of the list
-- and reversing the result at the end (after we trim the empty pots at the end
procreate :: [Plant] -> Instructions -> Int -> [Plant] -> ([Plant], Int)

--case: start
procreate (c:r1:r2:xs) instructions startingIndex [] = do
  let leftOfEdgeChar = getLeftOfEdgeChar c r1 instructions
  let updatedResult = (getCenterValue ('.':'.':c:r1:r2:[]) instructions):leftOfEdgeChar:[]
  procreate (c:r1:r2:xs) instructions (startingIndex - 1) updatedResult

--case second
procreate (l2:c:r1:r2:xs) instructions startingIndex (res1:res2:[]) = do
  let updatedResult = (getCenterValue ('.':l2:c:r1:r2:[]) instructions):res1:res2:[]
  procreate (l2:c:r1:r2:xs) instructions startingIndex updatedResult

--case: beforeLast
procreate (l1:l2:c:r1:[]) instructions startingIndex result = do
  let updatedResult = (getCenterValue (l1:l2:c:r1:'.':[]) instructions):result
  procreate (l2:c:r1:[]) instructions startingIndex updatedResult

--case: last
procreate (l1:l2:c:[]) instructions startingIndex result = do
  let outerValue = getCenterValue (l1:l2:c:'.':'.':[]) instructions
  let rightEdge = getCenterValue (l2:c:'.':'.':'.':[]) instructions
  let updatedResult = trimEmptyPotsAtEdge (rightEdge:outerValue:result)
  trimEmptyPotsAtEdgeWithIndex (reverse updatedResult) startingIndex


procreate (l1:l2:c:r1:r2:xs) instructions startingIndex result = do
  let updatedResult = (getCenterValue (l1:l2:c:r1:r2:[]) instructions):result
  procreate (l2:c:r1:r2:xs) instructions startingIndex updatedResult

getLeftOfEdgeChar :: Plant -> Plant -> Instructions -> Plant
getLeftOfEdgeChar c1 r1 instructions = getCenterValue ('.':'.':'.':c1:r1:[]) instructions

trimEmptyPotsAtEdge :: [Plant] -> [Plant]
trimEmptyPotsAtEdge (x:xs)
  | x == '#' = x:xs
  |otherwise = trimEmptyPotsAtEdge xs

trimEmptyPotsAtEdgeWithIndex :: [Plant] -> Int -> ([Plant], Int)
trimEmptyPotsAtEdgeWithIndex (x:xs) index
  | x == '#' = (x:xs, index)
  |otherwise = trimEmptyPotsAtEdgeWithIndex xs (index + 1)

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
