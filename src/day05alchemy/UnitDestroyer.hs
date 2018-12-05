import Data.Map
import Data.Char
import Data.List

-- 0m28.920s
main = do
  contents <- readFile "inputDay5.txt"
  let stableResult = reactToStableResult contents
  print (Data.Map.size stableResult)

  print (findQuickestResult (elems stableResult))

findQuickestResult contents = do
  let charToPolymerSize = getPolymerSizesForPolymersWithoutChar contents
  head (sortOn (snd) charToPolymerSize)

getPolymerSizesForPolymersWithoutChar :: String -> [(Char, Int)]
getPolymerSizesForPolymersWithoutChar polymer = Prelude.map (\x -> reactWithoutChar x polymer) ['a'..'z']

reactWithoutChar :: Char -> String -> (Char, Int)
reactWithoutChar char polymer = do
   let filteredPolymer = Data.List.filter (/= (toUpper char)) (Data.List.filter (/= char) polymer)
   (char, Data.Map.size (reactToStableResult filteredPolymer))

-----------------
--REACTIONS
-----------------
reactToStableResult polymer = do
  let idsWithUnits = reverse (ideify polymer [])
  causeChainReaction idsWithUnits (fromList idsWithUnits)

causeChainReaction :: [(Int, Char)] -> Map Int Char -> Map Int Char
causeChainReaction [x] remainingUnits = remainingUnits
causeChainReaction (x:y:[]) remainingUnits
  | shouldReact (snd x) (snd y) = Data.Map.delete (fst x) (Data.Map.delete (fst y) remainingUnits)
  | otherwise = remainingUnits
causeChainReaction (x:y:xs) remainingUnits
  | shouldReact (snd x) (snd y) = react (fst x) (fst y) remainingUnits
  | otherwise = causeChainReaction (y:xs) remainingUnits

react :: Int -> Int -> Map Int Char -> Map Int Char
react x y units = do
  let remainingUnits = (Data.Map.delete x (Data.Map.delete y units))
  causeChainReaction (toList remainingUnits) remainingUnits

ideify :: String -> [(Int, Char)] -> [(Int, Char)]
ideify (x:xs) [] = ideify xs [(0, x)]
ideify [x] idedChars = ((fst (head idedChars) + 1), x) : idedChars
ideify (x:xs) idedChars = ideify xs (((fst (head idedChars) + 1), x): idedChars)

shouldReact :: Char -> Char -> Bool
shouldReact x y = abs ((ord x) - (ord y)) == 32