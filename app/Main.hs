module Main where

import AOC.Day15.GoblinSlaying

main :: IO ()
main = do
  content <- readFile "resources/input-day15.txt"
--  let initialBattleCave = parseBattleCave content 3
--  let resolvedConflict = resolveCaveConflict initialBattleCave 0
--  print $ fst resolvedConflict
--  print $ snd resolvedConflict
  print $ findFirstTimeAllElvesSurvive content 15
--  printCaveConflict (parseBattleCave randomTest) 0

randomTest = "#######\n"++
             "#######\n"++
             "#.E..G#\n"++
             "#.#####\n"++
             "#G#####\n"++
             "#######\n"++
             "#######\n"

--------------0123456
testInput =  "#######\n"++ -- 0
             "#.G...#\n"++ -- 1
             "#...EG#\n"++ -- 2
             "#.#.#G#\n"++ -- 3
             "#..G#E#\n"++ -- 4
             "#.....#\n"++
             "#######\n"

------------------0123456
otherTestInput = "#######\n"++
                 "#G..#E#\n"++
                 "#E#E.E#\n"++
                 "#G.##.#\n"++
                 "#...#E#\n"++
                 "#...E.#\n"++
                 "#######\n"

movementTestInput = "#########\n"++
                    "#G..G..G#\n"++
                    "#.......#\n"++
                    "#.......#\n"++
                    "#G..E..G#\n"++
                    "#.......#\n"++
                    "#.......#\n"++
                    "#G..G..G#\n"++
                    "#########\n"


-- answer is between 182715 and 185526