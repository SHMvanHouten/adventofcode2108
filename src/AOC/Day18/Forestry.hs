module AOC.Day18.Forestry where

import AOC.Util.Coordinate
import Data.Map (Map, fromList, member, (!), findMin, findMax, empty, elems, fromListWith)
import Data.Maybe (isJust, fromJust)
import Data.List (groupBy)

treesTimesLumberYards acres = (length $ acreByAcreType!Tree) * (length $ acreByAcreType!Lumberyard)
 where acreByAcreType = getAcreByAcreType acres

getAcreByAcreType acres = fromListWith (++)
                          $ map (\a -> ((acreType a), [a]))
                          $ elems acres


evolveMinutes amount acres
  | amount == 0 = acres
  | otherwise = evolveMinutes (amount - 1) (evolveMinute acres)

evolveMinute acres = do
      let justAcres = elems acres
      fromList
            $ map (\a -> (coordinate a, a))
            $ map (\a-> evolveAcre a acres) justAcres

evolveAcre :: Acre -> Map Coordinate Acre -> Acre
evolveAcre acre surroundingAcres
  | aType == Open = evolveOpen acre surroundingAcres
  | aType == Tree = evolveTree acre surroundingAcres
  | aType == Lumberyard = evolveLumberYard acre surroundingAcres
  where aType = acreType acre

evolveOpen :: Acre -> Map Coordinate Acre -> Acre
evolveOpen acre surroundingAcres
  | countTrees circleAroundAcre >= 3 = toTree acre
  | otherwise = acre
  where circleAroundAcre = getSurroundingAcres (coordinate acre) surroundingAcres

evolveTree :: Acre -> Map Coordinate Acre -> Acre
evolveTree acre surroundingAcres
  | countLumberYards circleAroundAcre >= 3 = toLumberYard acre
  | otherwise = acre
  where circleAroundAcre = getSurroundingAcres (coordinate acre) surroundingAcres

evolveLumberYard :: Acre -> Map Coordinate Acre -> Acre
evolveLumberYard acre surroundingAcres
  | (countLumberYards circleAroundAcre >= 1) && (countTrees circleAroundAcre >= 1) = acre
  | otherwise = toOpen acre
  where circleAroundAcre = getSurroundingAcres (coordinate acre) surroundingAcres

countLumberYards circleAroundAcre = countAcreTypes circleAroundAcre Lumberyard
countTrees circleAroundAcre = countAcreTypes circleAroundAcre Tree
countAcreTypes circleAroundAcre aType = length $ filter (\a -> (acreType a) == aType) $ map (fromJust) $ filter (isJust) circleAroundAcre

toTree acre = Acre (coordinate acre) Tree
toLumberYard acre = Acre (coordinate acre) Lumberyard
toOpen acre = Acre (coordinate acre) Open

getSurroundingAcres coord acres = map (\c -> getMaybeAcre c acres) $ getCircleAroundCoordinate coord

getMaybeAcre coord acres
  | not $ coord `member` acres = Nothing
  | otherwise = Just (acres!coord)

data Acre = Acre {
  coordinate :: Coordinate,
  acreType :: AcreType
} deriving (Eq, Show, Ord)

data AcreType = Tree | Open | Lumberyard deriving (Eq, Show, Ord)

----------------------
-- Parse and print
----------------------

parseInput rawString = parseLines (lines rawString) 0 []

parseLines :: [String] -> Int -> [[Acre]] -> Map Coordinate Acre
parseLines [] _ acres = fromList $ map (\a -> (coordinate a, a )) $ concat $ reverse acres
parseLines (line:lines) y acreLines = parseLines lines (y + 1) ((parseAcres line 0 y []): acreLines)

parseAcres :: [Char] -> Int -> Int -> [Acre] -> [Acre]
parseAcres [] _ _ acres = reverse acres
parseAcres (char:chars) x y acres = parseAcres chars (x + 1) y ((toAcre x y char): acres)

toAcre :: Int -> Int -> Char -> Acre
toAcre x y char = Acre (Coordinate x y) (toAcreType char)

toAcreType :: Char -> AcreType
toAcreType char
  | char == '.' = Open
  | char == '|' = Tree
  | char == '#' = Lumberyard

drawLumberMap :: Map Coordinate Acre -> String
drawLumberMap acres = unlines $ map (\y -> drawLine acres [xMin..xMax] y) [yMin..yMax]
  where (xMin, yMin) = getXY $ fst $ findMin acres
        (xMax, yMax) = getXY $ fst $ findMax acres

drawLine acres xRange y = map (\x -> toChar $ acres!(Coordinate x y)) xRange
toChar acre
  | aType == Open = '.'
  | aType == Tree = '|'
  | aType == Lumberyard = '#'
  where aType = acreType acre