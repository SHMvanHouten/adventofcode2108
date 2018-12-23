module AOC.Day19.Reversing where

import Data.Map (Map, fromList, (!), empty, insert, member)
import AOC.Day16.Registers

runTheProgram :: Instructions -> Registers -> Int -> Registers
runTheProgram instructions registers ipRegister
  | updatedInstructionPointer `member` instructions = runTheProgram instructions registersWithUpdatedIp ipRegister
  | otherwise = resultingRegisters
  where instructionPointer = registers!ipRegister
        func = actionFunction (instructions!instructionPointer)
        resultingRegisters = func registers
        updatedInstructionPointer = (resultingRegisters!ipRegister) + 1
        registersWithUpdatedIp = insert ipRegister updatedInstructionPointer resultingRegisters

type Ip = Int
data Instruction = Instruction {
  ip :: Ip,
  actionFunction :: ActionFunction
}
type Instructions = Map Ip Instruction
--------------------
-- Parse
--------------------

type ActionFunction = Registers -> Registers

parseRegisterInput raw = do
  let rawLines = lines raw
  let ipRegister = parseIpRegister $ head rawLines
  let instructions = parseInstructions $ tail rawLines
  (ipRegister, instructions)

parseIpRegister rawLine = parseInt (last $ words rawLine)

parseInstructions rawLines = fromList $ map (toInstruction) $ zip [0..] rawLines

toInstruction (ip, rawInstruction) = do
  let splitLine = words rawInstruction
  let name = head splitLine
  let theAction = toAction' $ tail splitLine
  let opFunc = allNamesToOperationFunctions!name
  let actionFunction = opFunc theAction
  (ip, Instruction ip actionFunction)

toAction' rawInts = do
  let (a,b,c) = getThree $ map (parseInt) rawInts
  Action (-1) a b c

getThree list = (list!!0, list!!1, list!!2)

allNamesToOperationFunctions = fromList $ map (\o -> (name o, function o)) allOperationObjects