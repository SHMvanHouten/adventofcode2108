module AOC.Day16.RegistersSpec where

import Test.Hspec
import Test.QuickCheck

import AOC.Day16.Registers
import Data.Map (Map, (!), fromList, insert)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "challenge part 2" $ do
    it "solves challenge part 2" $ do
      content <- readFile "resources/input-day16.txt"
      let result = runProgram content
      result!0 `shouldBe` 577

  describe "it should solve the challenge part 1" $ do
    it "solves the challenge" $ do
      content <- readFile "resources/input-day16.txt"
      let instructions = parseInput content
      let result = findInstructionsWithMoreThanThreePossibleOpcodes instructions
      length result `shouldBe` 607

  describe "it should match the op codes to operations" $ do
    it "matches the op codes to operations" $ do
      content <- readFile "resources/input-day16.txt"
      let instructions = parseInput content
      let opCodesToOperation = findOpCodesForOperations instructions
      let result = map (\x -> (fst x, (name $ snd x))) opCodesToOperation
      result `shouldBe` [(5,"bani"),(15,"banr"),(7,"setr"),(14,"gtir"),(11,"eqrr"),(6,"gtri"),(8,"gtrr"),(10,"eqir"),(13,"eqri"),(9,"seti"),(2,"addi"),(3,"muli"),(0,"bori"),(1,"borr"),(4,"addr"),(12,"mulr")]

  describe "hasThreeOrMoreOpcodes" $ do
    it "matches three op codes" $ do
      let instruction = Instruction (fromList[(0, 3), (1, 2), (2, 1), (3, 1)]) (Action 9 2 1 2) (fromList[(0, 3), (1, 2), (2, 2), (3, 1)])
      hasThreeOrMoreOpcodes instruction `shouldBe` True
    --Before: [3, 2, 1, 1]
    --9 2 1 2
    --After:  [3, 2, 2, 1]

  describe "addr" $ do
    it "stores into register C the result of adding register A and _register_ B" $ do
      addr (fromList[(0, 1), (1, 2), (2, 0), (3, 3)]) (Action 14 0 3 2) `shouldBe` (fromList[(0, 1), (1, 2), (2, 4), (3, 3)])

  describe "addi" $ do
    it "stores into register C the result of adding register A and _value_ B" $ do
      addi (fromList[(0, 1), (1, 2), (2, 0), (3, 3)]) (Action 14 0 3 2) `shouldBe` (fromList[(0, 1), (1, 2), (2, 4), (3, 3)])

  describe "mulr" $ do
    it "stores into register C the result of multiplying register A and _register_ B" $ do
      mulr (fromList[(0, 1), (1, 2), (2, 0), (3, 3)]) (Action 14 0 3 2) `shouldBe` (fromList[(0, 1), (1, 2), (2, 3), (3, 3)])

  describe "muli" $ do
    it "stores into register C the result of multiplying register A and _value_ B" $ do
      muli (fromList[(0, 1), (1, 2), (2, 0), (3, 3)]) (Action 14 0 3 2) `shouldBe` (fromList[(0, 1), (1, 2), (2, 3), (3, 3)])

  describe "banr" $ do
    it "stores into register C the result of the bitwise AND of register A and _register_ B" $ do
      banr (fromList[(0, 1), (1, 2), (2, 0), (3, 3)]) (Action 14 0 3 2) `shouldBe` (fromList[(0, 1), (1, 2), (2, 1), (3, 3)])

  describe "bani" $ do
    it "stores into register C the result of the bitwise AND of register A and _value_ B" $ do
      bani (fromList[(0, 1), (1, 2), (2, 0), (3, 3)]) (Action 14 0 3 2) `shouldBe` (fromList[(0, 1), (1, 2), (2, 1), (3, 3)])

  describe "borr" $ do
    it "stores into register C the result of the bitwise OR of register A and _register_ B" $ do
      borr (fromList[(0, 1), (1, 2), (2, 0), (3, 3)]) (Action 14 0 3 2) `shouldBe` (fromList[(0, 1), (1, 2), (2, 3), (3, 3)])

  describe "bori" $ do
    it "stores into register C the result of the bitwise OR of register A and _value_ B" $ do
      bori (fromList[(0, 2), (1, 2), (2, 0), (3, 3)]) (Action 14 0 4 2) `shouldBe` (fromList[(0, 2), (1, 2), (2, 6), (3, 3)])

  describe "setr" $ do
    it "copies the contents of _register_ A into register C. (Input B is ignored.)" $ do
      setr (fromList[(0, 2), (1, 2), (2, 0), (3, 3)]) (Action 14 0 4 2) `shouldBe` (fromList[(0, 2), (1, 2), (2, 2), (3, 3)])

  describe "seti" $ do
    it "(set immediate) stores _value_ A into register C. (Input B is ignored.)" $ do
      seti (fromList[(0, 2), (1, 2), (2, 0), (3, 3)]) (Action 14 0 4 2) `shouldBe` (fromList[(0, 2), (1, 2), (2, 0), (3, 3)])

  describe "gtir (greater-than immediate/register)" $ do
    it "sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0" $ do
      gtir (fromList[(0, 2), (1, 2), (2, 0), (3, 3)]) (Action 14 3 3 2) `shouldBe` (fromList[(0, 2), (1, 2), (2, 0), (3, 3)])

  describe "gtri (greater-than register/immediate)" $ do
    it "sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0." $ do
      gtri (fromList[(0, 2), (1, 2), (2, 0), (3, 3)]) (Action 14 3 3 2) `shouldBe` (fromList[(0, 2), (1, 2), (2, 0), (3, 3)])

  describe "gtrr (greater-than register/register)" $ do
    it "sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0." $ do
      gtrr (fromList[(0, 2), (1, 1), (2, 0), (3, 3)]) (Action 14 0 1 2) `shouldBe` (fromList[(0, 2), (1, 1), (2, 1), (3, 3)])

  describe "eqir (equal immediate/register)" $ do
    it "sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0." $ do
      eqir (fromList[(0, 2), (1, 1), (2, 0), (3, 3)]) (Action 14 0 1 2) `shouldBe` (fromList[(0, 2), (1, 1), (2, 0), (3, 3)])

  describe "eqri (equal register/immediate)" $ do
    it "sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0." $ do
      eqri (fromList[(0, 2), (1, 1), (2, 0), (3, 3)]) (Action 14 0 2 2) `shouldBe` (fromList[(0, 2), (1, 1), (2, 1), (3, 3)])

  describe "eqrr (equal register/register)" $ do
    it "sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0." $ do
      eqrr (fromList[(0, 2), (1, 1), (2, 0), (3, 3)]) (Action 14 0 2 2) `shouldBe` (fromList[(0, 2), (1, 1), (2, 0), (3, 3)])

  describe "parse input" $ do
    it "parses the input to instructions" $ do
      let input = "Before: [1, 2, 0, 3]\n"++
                  "14 0 3 2\n"++
                  "After:  [1, 2, 0, 3]\n"++
                  "\n"++
                  "\n"++
                  "\n"++
                  "9 2 0 0\n"++
                  "9 3 0 2\n"++
                  "9 1 0 3\n"++
                  "12 3 0 2\n"++
                  "3 2 1 2\n"
      parseInput input `shouldBe` [Instruction (fromList[(0, 1), (1, 2), (2, 0), (3, 3)]) (Action 14 0 3 2) (fromList[(0, 1), (1, 2), (2, 0), (3, 3)])]
