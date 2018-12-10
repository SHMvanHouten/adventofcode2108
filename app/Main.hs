module Main where

import AOC.Day07.BuildingASleigh
import AOC.Day08.LicenseTree
import AOC.Day09.MarbleGame
import AOC.Day10.StarMessage

main :: IO ()
main = do
  contents <- readFile "/Users/SHMvanHouten/Projects/Haskell/adventofcode2018/resources/input-day10.txt"
  let stars = parseStars contents
  print $ drawSky $tickNTimes stars 10515
--  let starsToIters = moveStarsUntilMinimumWidth stars 0 1000000 []
--  print (snd starsToIters)
--  print (drawSky $ fst starsToIters)
--  let stars = parseStars testInput
--  let tickedStars = tick $ tick $ tick $ tick stars
--  print tickedStars
--  print $ drawSky (fst $ moveStarsUntilMinimumHeight stars 0 100 [])

