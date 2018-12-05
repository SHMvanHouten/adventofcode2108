module Frequency where

main = do
        contents <- readFile "input"
        let frequencyShifts = map parseInt (map removePlus (words contents))
        print (sum frequencyShifts)

removePlus input = if input !! 0 == '+'
                        then tail input
                        else input

parseInt str = read str :: Int
