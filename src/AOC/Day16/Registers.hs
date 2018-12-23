module AOC.Day16.Registers where

import qualified Data.List.Split as Split
import qualified Data.Bits as Bits
import Data.Map (Map, (!), fromList, insert, fromListWith, map, keys)
import qualified Data.List as List
import qualified Data.Set as Set

initialRegisters = fromList [(0, 0),(1, 0), (2, 0), (3, 0)]

runProgram :: String -> Registers
runProgram rawInstructions = do
  let (instructions, actions) = parseInputForPart2 rawInstructions
  let opCodesToOperation = fromList $ findOpCodesForOperations instructions
  runAction initialRegisters actions opCodesToOperation

runAction :: Registers -> [Action] -> Map Int Operation -> Registers
runAction valueBefore [] _ = valueBefore
runAction valueBefore (action:actions) opCodesToOperation = do
  let operation = function $ opCodesToOperation!(identity action)
  let valueAfter = operation action valueBefore
  runAction valueAfter actions opCodesToOperation


findOpCodesForOperations instructions = do
  let groupedOpCodesToOperation = groupOpCodeByOperations instructions
  matchOpCodeToOperation (keys groupedOpCodesToOperation) groupedOpCodesToOperation [] []

matchOpCodeToOperation :: [Int] -> Map Int (Set.Set Operation) -> [(Int, Operation)] -> [Int] -> [(Int, Operation)]
matchOpCodeToOperation [] opCodeToOperations matches unmatchedCodes = matchOpCodeToOperation unmatchedCodes opCodeToOperations matches unmatchedCodes
matchOpCodeToOperation (opCode:opCodes) opCodeToOperations matches unmatchedCodes
  | (length matches) == (length allOperations) = matches
  | (Set.size $ opCodeToOperations!opCode) == 1 = do
    let operation = opCodeToOperations!opCode
    matchOpCodeToOperation opCodes (Data.Map.map (\x -> Set.delete (Set.elemAt 0 operation) x) opCodeToOperations) ((opCode, (Set.elemAt 0 operation)):matches) unmatchedCodes
  | otherwise = matchOpCodeToOperation opCodes opCodeToOperations matches (opCode:unmatchedCodes)

groupOpCodeByOperations instructions = do
  let opCodeByOperations = Prelude.map (getOpCodeToOperations) instructions
  fromListWith (Set.union) opCodeByOperations

getOpCodeToOperations :: InstructionExample -> (Int, Set.Set Operation)
getOpCodeToOperations instruction = (identity $ action' $ instruction, findMatchingOperations instruction)

--------------------------
-- PART 1
--------------------------

findInstructionsWithMoreThanThreePossibleOpcodes :: [InstructionExample] -> [InstructionExample]
findInstructionsWithMoreThanThreePossibleOpcodes instructions = filter (hasThreeOrMoreOpcodes) instructions

hasThreeOrMoreOpcodes instruction = (length $ findMatchingOperations instruction) >= 3

findMatchingOperations :: InstructionExample -> Set.Set Operation
findMatchingOperations instruction = Set.fromList $ filter (\op -> actionOnBeforeMatchesAfter instruction (function op)) allOperationObjects

actionOnBeforeMatchesAfter :: InstructionExample -> OperationFunction -> Bool
actionOnBeforeMatchesAfter instruction operation = ((action' instruction) `operation` (before instruction)) == (after instruction)
----------------------
--  OPERATIONS
----------------------
addr :: Action -> Registers -> Registers
addr action registers = doOperationToRegisters registers action (+)
addi action registers = doOperationToRegisterAndValue registers action (+)

mulr action registers = doOperationToRegisters registers action (*)
muli action registers = doOperationToRegisterAndValue registers action (*)

banr :: Action -> Registers -> Registers
banr action registers = doOperationToRegisters registers action (Bits..&.)
bani action registers = doOperationToRegisterAndValue registers action (Bits..&.)

borr :: Action -> Registers -> Registers
borr action registers = doOperationToRegisters registers action (Bits..|.)
bori action registers = doOperationToRegisterAndValue registers action (Bits..|.)

setr action registers = insert (c action) (registers!(a action)) registers
seti action registers = insert (c action) (a action) registers

gtir :: Action -> Registers -> Registers
gtir action registers = insert (c action) ((a action) `isGreaterThan` (registers!(b action))) registers
gtri action registers = insert (c action) ((registers!(a action)) `isGreaterThan` (b action)) registers
gtrr action registers = insert (c action) ((registers!(a action)) `isGreaterThan` (registers!(b action))) registers

eqir action registers = insert (c action) ((a action) `isEqualTo` (registers!(b action))) registers
eqri action registers = insert (c action) ((registers!(a action)) `isEqualTo` (b action)) registers
eqrr action registers = insert (c action) ((registers!(a action)) `isEqualTo` (registers!(b action))) registers

isGreaterThan value1 value2
  | value1 > value2 = 1
  | otherwise = 0

isEqualTo value1 value2
  | value1 == value2 = 1
  | otherwise = 0

doOperationToRegisters registers action operation = do
  let registerA = registers!(a action)
  let registerB = registers!(b action)
  insert (c action) (registerA `operation` registerB) registers

doOperationToRegisterAndValue registers action operation = do
  let registerA = registers!(a action)
  let valueB = b action
  insert (c action) (registerA `operation` valueB) registers
-------------------
-- PARSE
-------------------

toInstructionExample rawInstruction = do
  let rawInstructionSplit = lines rawInstruction
  let before = toDeviceState (rawInstructionSplit!!0)
  let action = toAction (rawInstructionSplit!!1)
  let after = toDeviceState (rawInstructionSplit!!2)
  InstructionExample before action after

toAction rawAction = do
  let splitAction = words rawAction
  Action (read (splitAction!!0) :: Int) (read (splitAction!!1) :: Int) (read (splitAction!!2) :: Int) (read (splitAction!!3) :: Int)

toDeviceState rawState = do
  let aThroughD = zip [0..] $ Prelude.map (parseInt)$ Split.splitOn ", " $ init $ (Split.splitOn "[" rawState)!!1
  fromList [(aThroughD!!0), (aThroughD!!1), (aThroughD!!2), (aThroughD!!3)]

parseInput :: String -> [InstructionExample]
parseInput rawInput = do
  let (rawInstructions, rawTestProgram) = splitIntoTwo rawInput
  let rawInstructionsSplit = Split.splitOn "\n\n" rawInstructions
  Prelude.map (toInstructionExample) rawInstructionsSplit

splitIntoTwo rawInput = do
  let splitList = Split.splitOn "\n\n\n\n" rawInput
  (splitList!!0, splitList!!1)

parseInputForPart2 :: String -> ([InstructionExample], [Action])
parseInputForPart2 rawInput = do
  let instructions = parseInput rawInput
  let rawProgram = snd $ splitIntoTwo rawInput
  (instructions, rawProgramToActions rawProgram)

rawProgramToActions :: String -> [Action]
rawProgramToActions rawProgram = do
  let actionLines = lines rawProgram
  Prelude.map (toAction) actionLines

data Action = Action {
  identity::Int,
  a::Int,
  b::Int,
  c::Int
}deriving(Show, Eq)

type Register = Int
type Registers = Map Int Register

type OperationFunction = Action -> Registers -> Registers

data InstructionExample = InstructionExample {
  before :: Registers,
  action' :: Action,
  after :: Registers
} deriving(Show, Eq)

data Operation = Operation {
  name :: String,
  function :: OperationFunction
}

instance Eq Operation where
    (Operation name1 _) == (Operation name2 _) = name1 == name2
instance Show Operation where
  show (Operation name _) = show name
instance Ord Operation where
  compare (Operation name1 _) (Operation name2 _) = compare name1 name2

parseInt str = read str :: Int

allOperationObjects :: [Operation]
allOperationObjects = [(Operation "addr" addr),
                        (Operation "addi" addi),
                        (Operation "mulr" mulr),
                        (Operation "muli" muli),
                        (Operation "banr" banr),
                        (Operation "bani" bani),
                        (Operation "borr" borr),
                        (Operation "bori" bori),
                        (Operation "setr" setr),
                        (Operation "seti" seti),
                        (Operation "gtir" gtir),
                        (Operation "gtri" gtri),
                        (Operation "gtrr" gtrr),
                        (Operation "eqir" eqir),
                        (Operation "eqri" eqri),
                        (Operation "eqrr" eqrr)]

allOperations = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]
