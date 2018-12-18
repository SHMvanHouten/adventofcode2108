module AOC.Day16.Registers where

import qualified Data.List.Split as Split
import qualified Data.Bits as Bits
import Data.Map (Map, (!), fromList, insert, fromListWith, map, keys)
import qualified Data.List as List
import qualified Data.Set as Set

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

getOpCodeToOperations :: Instruction -> (Int, Set.Set Operation)
getOpCodeToOperations instruction = (identity $ action $ instruction, findMatchingOperations instruction)

--------------------------
-- PART 1
--------------------------

findInstructionsWithMoreThanThreePossibleOpcodes :: [Instruction] -> [Instruction]
findInstructionsWithMoreThanThreePossibleOpcodes instructions = filter (hasThreeOrMoreOpcodes) instructions

hasThreeOrMoreOpcodes instruction = (length $ findMatchingOperations instruction) >= 3

findMatchingOperations :: Instruction -> Set.Set Operation
findMatchingOperations instruction = Set.fromList $ filter (\op -> actionOnBeforeMatchesAfter instruction (function op)) allOperationObjects

actionOnBeforeMatchesAfter :: Instruction -> OperationFunction -> Bool
actionOnBeforeMatchesAfter instruction operation = ((before instruction) `operation` (action instruction)) == (after instruction)
----------------------
--  OPERATIONS
----------------------
allOperations = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

addr :: Registers -> Action -> Registers
addr registers action = doOperationToRegisters registers action (+)
addi registers action = doOperationToRegisterAndValue registers action (+)

mulr registers action = doOperationToRegisters registers action (*)
muli registers action = doOperationToRegisterAndValue registers action (*)

banr :: Registers -> Action -> Registers
banr registers action = doOperationToRegisters registers action (Bits..&.)
bani registers action = doOperationToRegisterAndValue registers action (Bits..&.)

borr :: Registers -> Action -> Registers
borr registers action = doOperationToRegisters registers action (Bits..|.)
bori registers action = doOperationToRegisterAndValue registers action (Bits..|.)

setr registers action = insert (c action) (registers!(a action)) registers
seti registers action = insert (c action) (a action) registers

gtir :: Registers -> Action -> Registers
gtir registers action = insert (c action) ((a action) `isGreaterThan` (registers!(b action))) registers
gtri registers action = insert (c action) ((registers!(a action)) `isGreaterThan` (b action)) registers
gtrr registers action = insert (c action) ((registers!(a action)) `isGreaterThan` (registers!(b action))) registers

eqir registers action = insert (c action) ((a action) `isEqualTo` (registers!(b action))) registers
eqri registers action = insert (c action) ((registers!(a action)) `isEqualTo` (b action)) registers
eqrr registers action = insert (c action) ((registers!(a action)) `isEqualTo` (registers!(b action))) registers

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

toInstruction rawInstruction = do
  let rawInstructionSplit = lines rawInstruction
  let before = toDeviceState (rawInstructionSplit!!0)
  let action = toAction (rawInstructionSplit!!1)
  let after = toDeviceState (rawInstructionSplit!!2)
  Instruction before action after

toAction rawAction = do
  let splitAction = words rawAction
  Action (read (splitAction!!0) :: Int) (read (splitAction!!1) :: Int) (read (splitAction!!2) :: Int) (read (splitAction!!3) :: Int)

toDeviceState rawState = do
  let aThroughD = zip [0..] $ Prelude.map (parseInt)$ Split.splitOn ", " $ init $ (Split.splitOn "[" rawState)!!1
  fromList [(aThroughD!!0), (aThroughD!!1), (aThroughD!!2), (aThroughD!!3)]

parseInput :: String -> [Instruction]
parseInput rawInput = do
  let (rawInstructions, rawTestProgram) = splitIntoTwo rawInput
  let rawInstructionsSplit = Split.splitOn "\n\n" rawInstructions
  Prelude.map (toInstruction) rawInstructionsSplit

splitIntoTwo rawInput = do
  let splitList = Split.splitOn "\n\n\n" rawInput
  (splitList!!0, splitList!!1)

data Action = Action {
  identity::Int,
  a::Int,
  b::Int,
  c::Int
}deriving(Show, Eq)

type Register = Int
type Registers = Map Int Register

type OperationFunction = Registers -> Action -> Registers

data Instruction = Instruction {
  before :: Registers,
  action :: Action,
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
