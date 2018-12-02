import System.IO
import Data.Map
import Data.List.Split
import Data.List
import Data.Maybe

main = do
        contents <- readFile "boxids.txt"
        let boxids = splitOn "\n" contents
        let foundWordPair = findTheWord boxids
        let zippedWords = zip (fst foundWordPair) (snd foundWordPair)
        let sameLetters = filterSameLetters zippedWords
        print sameLetters

findTheWord :: [String] -> (String, String)
findTheWord [] = error "no word found"
findTheWord [x] = error "no word found"
findTheWord (x:xs)
                  | isJust foundWord = (x, fromJust foundWord)
                  | otherwise = findTheWord xs
                  where foundWord = find (hasOneDiffChar x) xs

hasOneDiffChar word1 word2 = countDiffChars (zip word1 word2) == 1

countDiffChars zippedWords = sum ([1 | pair <- zippedWords, (fst pair) /= (snd pair)])

filterSameLetters zippedWords = [fst pair | pair <- zippedWords, (fst pair) == (snd pair)]

