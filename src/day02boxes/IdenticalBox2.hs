import Data.Map
import Data.List
import Data.Maybe

main = do
        contents <- readFile "boxids.txt"
        let boxids = words contents
        print (findTheWord boxids)

findTheWord :: [String] -> String
findTheWord [] = error "no word found"
findTheWord [x] = error "no word found"
findTheWord (x:xs)
              | isJust foundSameLetters = fromJust foundSameLetters
              | otherwise = findTheWord xs
              where foundSameLetters = findOneCharDiff x xs


findOneCharDiff :: String -> [String] -> Maybe String
findOneCharDiff word [] = Nothing
findOneCharDiff word [y] = sameCharsIfOneCharDiff word y
findOneCharDiff word (y:ys)
                      | isJust foundWord = foundWord
                      | otherwise = findOneCharDiff word ys
                      where foundWord = sameCharsIfOneCharDiff word y


sameCharsIfOneCharDiff word1 word2
                        | length sameChars == ((length word1) -1) = Just sameChars
                        | otherwise = Nothing
                        where sameChars = filterSameLetters (zip word1 word2)

filterSameLetters zippedWords = [fst pair | pair <- zippedWords, (fst pair) == (snd pair)]
