module AOC.Day19.ReversingSpec where

import Test.Hspec
import Test.QuickCheck

import AOC.Day19.Reversing
import AOC.Util.Coordinate
import Data.Map (empty, fromList, (!))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "solves the challenge input" $ do
      it "solves the challenge input" $ do
        input <- readFile "resources/input-day19.txt"
        let (ipRegister, instructions) = parseRegisterInput input
        let registers = fromList [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0)]
        let result = runTheProgram instructions registers ipRegister
--        printProgram instructions registers ipRegister
        (result!0) `shouldBe` 1152

  describe "solves the challenge input part 2" $ do
      it "solves the challenge input (where register 0 starts at 1" $ do
        input <- readFile "resources/input-day19.txt"
        let (ipRegister, instructions) = parseRegisterInput input
        let registers = fromList [(0,1), (1,0), (2,0), (3,0), (4,0), (5,0)]
        printProgram instructions regist?ers ipRegister
        pending
--        let result = runTheProgram instructions registers ipRegister
--        print result
--        (result!0) `shouldBe` 1152

  describe "solves the test input" $ do
      it "solves the test input" $ do
        let testInput = "#ip 0\n"++
                        "seti 5 0 1\n"++
                        "seti 6 0 2\n"++
                        "addi 0 1 0\n"++
                        "addr 1 2 3\n"++
                        "setr 1 0 0\n"++
                        "seti 8 0 4\n"++
                        "seti 9 0 5\n"
        let (ipRegister, instructions) = parseRegisterInput testInput
        let registers = fromList [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0)]
        let result = runTheProgram instructions registers ipRegister
        print result
        (result!0) `shouldBe` 6

