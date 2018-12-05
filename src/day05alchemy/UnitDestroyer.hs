import Data.Map
import Data.Char

main = do
  contents <- readFile "inputDay5.txt"
  let idsWithUnits = reverse (ideify contents [])
  let stableResult = causeChainReaction idsWithUnits (fromList idsWithUnits)
  print (Data.Map.size stableResult)
  print (elems stableResult)

causeChainReaction :: [(Int, Char)] -> Map Int Char -> Map Int Char
causeChainReaction [x] remainingUnits = remainingUnits
causeChainReaction (x:y:[]) remainingUnits
  | shouldReact (snd x) (snd y) = delete (fst x) (delete (fst y) remainingUnits)
  | otherwise = remainingUnits
causeChainReaction (x:y:xs) remainingUnits
  | shouldReact (snd x) (snd y) = react (fst x) (fst y) remainingUnits
  | otherwise = causeChainReaction (y:xs) remainingUnits

react :: Int -> Int -> Map Int Char -> Map Int Char
react x y units = do
  let remainingUnits = (delete x (delete y units))
  causeChainReaction (toList remainingUnits) remainingUnits

ideify :: String -> [(Int, Char)] -> [(Int, Char)]
ideify (x:xs) [] = ideify xs [(0, x)]
ideify [x] idedChars = ((fst (head idedChars) + 1), x) : idedChars
ideify (x:xs) idedChars = ideify xs (((fst (head idedChars) + 1), x): idedChars)

shouldReact :: Char -> Char -> Bool
shouldReact x y = abs ((ord x) - (ord y)) == 32