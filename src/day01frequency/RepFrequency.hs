module RepFrequency where

import System.IO
import Data.List.Split

main = do
        contents <- readFile "input"
        let list = splitOn "\n" contents
        let frequencyShifts = map parseInt (map removePlus list)

        print findFirstRepeating 0 [] frequencyShifts


findFirstRepeating :: (Int, [Int], [Int]) -> Int
--findFirstRepeating frequency prevFreqs (x:xs) = do
--                                                  let nextFreq = frequency + x
--                                                  if nextFreq `elem` prevFreqs
--                                                        then return nextFreq
--                                                        else (findFirstRepeating nextFreq (prevFreqs ++ [nextFreq]) xs)

findFirstRepeating frequency [] (x:xs) = findFirstRepeating (frequency + x) [frequency + x] xs

findFirstRepeating frequency [y] (x:xs)
                                      | (frequency + x) == y = y
                                      | otherwise = findFirstRepeating (frequency + x) ([y, (frequency + x)]) xs

findFirstRepeating frequency (y:ys) (x:xs)
                                            | (frequency + x) `elem` y:ys = (frequency + x)
                                            | otherwise = findFirstRepeating (frequency + x) ((frequency + x) : y : ys) xs

removePlus input = if input !! 0 == '+'
                        then tail input
                        else input

parseInt str = read str :: Int

--splitOn :: (String, IO) -> a
--splitOn x y = print "get this out of the way"
--sum :: (Num a) => [a] -> a
--sum _ = 0
--sum (x:xs) = x + sum xs