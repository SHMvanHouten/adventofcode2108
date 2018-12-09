module Main where

import AOC.Day07.BuildingASleigh
import AOC.Day08.LicenseTree

main :: IO ()
main = do
--  contents <- readFile "/Users/SHMvanHouten/Projects/Haskell/adventofcode2018/src/AOC/Day07/inputday7.txt"
--  print (getInstructionOrder contents)
--  print (timeTheSleighBuild contents 60 5)
  contents <- readFile "/Users/SHMvanHouten/Projects/Haskell/adventofcode2018/resources/input-day8.txt"
  print (getSumOfAllMetaDataEntries contents)
  print (getValueOfRootNode contents)
