module Frequency where

import System.IO
import Data.List.Split

main = do
        contents <- readFile "input"
        let frequencyShifts = map parseInt (map removePlus (splitOn "\n" contents))
        print (sum frequencyShifts)

--frequency = do
--              contents <- readFile "input"
--              map parseInt (map removePlus (splitOn "\n" contents))

removePlus input = if input !! 0 == '+'
                        then tail input
                        else input

parseInt str = read str :: Int

--splitOn :: (String, IO) -> a
--splitOn x y = print "get this out of the way"
--sum :: (Num a) => [a] -> a
--sum _ = 0
--sum (x:xs) = x + sum xs
