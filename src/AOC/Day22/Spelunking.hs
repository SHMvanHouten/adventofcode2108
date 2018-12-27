module AOC.Day22.Spelunking where

import AOC.Util.Coordinate
import Data.Map (Map, fromList, insert, elems, (!), map, empty)

determineRisk :: Coordinate -> Coordinate -> Int -> Int
determineRisk start target depth = do
  let erosionLevels = insert target 0 $ getErosionLevelMap [(x' start)..(x' target)] [(y' start)..(y' target)] depth
  sum $ Prelude.map (`mod` 3) $ elems erosionLevels

toRiskLevel region
  | region == Rocky = 0
  | region == Wet = 1
  | region == Narrow = 2

toRegion erosionLevel
    | erosionLevel `mod` 3 == 0 = Rocky
    | erosionLevel `mod` 3 == 1 = Wet
    | erosionLevel `mod` 3 == 2 = Narrow

determineGeologicIndex coordinate erosionMap
  | coordinate == Coordinate 0 0 = 0
  | y' coordinate == 0 = (x' coordinate) * 16807
  | x' coordinate == 0 = (y' coordinate) * 48271
  | otherwise = (erosionMap !(moveLeft coordinate)) * (erosionMap !(moveUp coordinate))

determineErosionLevel geologicIndex depth = (geologicIndex + depth) `mod` 20183

getErosionLevelMap :: [Int] -> [Int] -> Int -> Map Coordinate Int
getErosionLevelMap xRange yRange depth = toErosionLevelMap xRange yRange depth empty

toErosionLevelMap :: [Int] -> [Int] -> Int -> Map Coordinate Int -> Map Coordinate Int
toErosionLevelMap _ [] _ erosionLevelMap = erosionLevelMap
toErosionLevelMap xRange (y:ys) depth geoLogicIndexMap = toErosionLevelMap xRange ys depth (toGeologicIndexMapLine xRange y depth geoLogicIndexMap)

toGeologicIndexMapLine :: [Int] -> Int -> Int -> Map Coordinate Int -> Map Coordinate Int
toGeologicIndexMapLine [] _ _ geologicIndexMap = geologicIndexMap
toGeologicIndexMapLine (x:xs) y depth geologicIndexMap = toGeologicIndexMapLine xs y depth (insert coordinate (determineErosionLevel geologicIndex depth) geologicIndexMap)
  where coordinate = Coordinate x y
        geologicIndex = determineGeologicIndex coordinate geologicIndexMap

data Region = Rocky | Wet | Narrow deriving (Eq, Ord, Show)