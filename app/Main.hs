module Main where

import AOC.Day07.BuildingASleigh

main :: IO ()
main = do
  contents <- readFile "/Users/SHMvanHouten/Projects/Haskell/adventofcode2018/src/AOC/Day07/inputday7.txt"
  print (getInstructionOrder contents)


