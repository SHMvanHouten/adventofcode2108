module Main where

import AOC.Day12.PlantSpreading

main :: IO ()
main = do
  contents <- readFile "resources/input-day12-other.txt"
  let input = parseRawPlantInstructions contents
  passAndPrintGenerations 0 (fst input) ((snd input), 0) 100000

  let plantsToStartingIndex = passGenerations 9999 (fst input) ((snd input), 0)
  let valueOf10000Gererations = calculateTotalPlantValue (fst plantsToStartingIndex) (snd plantsToStartingIndex)

  print valueOf10000Gererations
  print "this is our answer:"
  print $ calculateTotalPlantValue (fst plantsToStartingIndex) 1000
