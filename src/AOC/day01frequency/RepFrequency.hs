module RepFrequency where
import Data.Set

main = do
        contents <- readFile "input"
        let list = words contents
        let frequencyShifts = Prelude.map parseInt (Prelude.map removePlus list)

        print (findFirstRepeating 0 Data.Set.empty (cycle frequencyShifts))

findFirstRepeating :: Int -> Set Int -> [Int] -> Int

findFirstRepeating frequency prevFreqs (x:xs)
                                        | nextFreq `Data.Set.member` prevFreqs = nextFreq
                                        | otherwise = findFirstRepeating nextFreq (Data.Set.insert (frequency + x) prevFreqs) xs
                                        where nextFreq = frequency + x

findFirstRepeating frequency prevFreqs [] = error "exhausted shifts"

removePlus input = if input !! 0 == '+'
                        then tail input
                        else input

parseInt str = read str :: Int