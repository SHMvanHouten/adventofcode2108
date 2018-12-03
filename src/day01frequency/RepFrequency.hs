--import Data.Set todo: find this somewhere cabal/hackage
-- works but super slow
-- real	2m17.641s
main = do
        contents <- readFile "input"
        let list = words contents
        let frequencyShifts = map parseInt (map removePlus list)

        print (findFirstRepeating 0 Set (cycle frequencyShifts))

findFirstRepeating :: Int -> [Int] -> [Int] -> Int

findFirstRepeating frequency [] (x:xs) = findFirstRepeating (frequency + x) [frequency + x] xs

findFirstRepeating frequency prevFreqs (x:xs)
                                        | nextFreq `elem` prevFreqs = nextFreq
                                        | otherwise = findFirstRepeating nextFreq (nextFreq : prevFreqs) xs
                                        where nextFreq = frequency + x

findFirstRepeating frequency prevFreqs [] = error "exhausted shifts"

removePlus input = if input !! 0 == '+'
                        then tail input
                        else input

parseInt str = read str :: Int

--sum :: (Num a) => [a] -> a
--sum _ = 0
--sum (x:xs) = x + sum xs