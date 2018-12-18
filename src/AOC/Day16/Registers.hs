module AOC.Day16.Registers where

import qualified Data.List.Split as Split
import qualified Data.Bits as Bits
import Data.Map (Map, (!), fromList, insert)
import qualified Data.List as List


groupOperationsByOpCode instructions = do
  let opCodeByOperations = map (getOpCodeToOperations) instructions
  ""

getOpCodeToOperations instruction = (identity $ action $ instruction, findMatchingOperations)

--------------------------
-- PART 1
--------------------------

findInstructionsWithMoreThanThreePossibleOpcodes :: [Instruction] -> [Instruction]
findInstructionsWithMoreThanThreePossibleOpcodes instructions = filter (hasThreeOrMoreOpcodes) instructions

hasThreeOrMoreOpcodes instruction = (length $ findMatchingOperations instruction) >= 3

findMatchingOperations instruction = filter (\op -> actionOnBeforeMatchesAfter instruction op) allOperations

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
  let aThroughD = zip [0..] $ map (parseInt)$ Split.splitOn ", " $ init $ (Split.splitOn "[" rawState)!!1
  fromList [(aThroughD!!0), (aThroughD!!1), (aThroughD!!2), (aThroughD!!3)]

parseInput :: String -> [Instruction]
parseInput rawInput = do
  let (rawInstructions, rawTestProgram) = splitIntoTwo rawInput
  let rawInstructionsSplit = Split.splitOn "\n\n" rawInstructions
  map (toInstruction) rawInstructionsSplit

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

data Instruction = Instruction {
  before :: Registers,
  action :: Action,
  after :: Registers
} deriving(Show, Eq)

parseInt str = read str :: Int