module AOC.Day23.NanoBots where

import AOC.Util.Coord3D
import Data.List.Split
import qualified Data.List as List
import Data.Sequence

findPointInRangeOfMostBots bots = do
  let coordsInRange = getCoordsInRange [10..50] [10..50] [10..50]
  findPointWithMostBots coordsInRange bots (stubCoord, 0)

findPointWithMostBots :: Seq Coord3d -> [NanoBot] -> (Coord3d, Int) -> Coord3d
findPointWithMostBots (c:<|cs) bots (bestPoint, value) = do
  let currentValue = List.length $ List.filter (`isInRange` c) bots
  if currentValue > value
    then findPointWithMostBots cs bots (c, currentValue)
    else findPointWithMostBots cs bots (bestPoint, value)
findPointWithMostBots empty _ (bestPoint, _) = bestPoint

isInRange (NanoBot loc r) coordinate = (distance loc coordinate) <= r

getAmountOfBotsInRadius :: NanoBot -> [NanoBot] -> Int
getAmountOfBotsInRadius bot others = List.length $ getBotsWithinRadius bot others

getBotsWithinRadius (NanoBot {coord= coordinate, radius = radius}) others=
  List.filter (\c -> coordinate `distance` c <= radius) $ map (coord) others

---------
-- Parse Input
---------

parseInput raw = map (parseBot)
                $ lines raw

parseBot raw = do
  let splitLines = words raw
  let coord = parseCoord3d $ head splitLines
  let radius = parseInt $ last $ splitOn "=" $ last splitLines
  NanoBot coord radius

parseCoord3d raw = do
  let rawCoords = head $ splitOn ">" $ List.drop 5 raw
  let coords = map (parseInt) $ splitOn "," rawCoords
  Coord3d (coords!!0) (coords!!1) (coords!!2)

data NanoBot = NanoBot {
  coord :: Coord3d,
  radius :: Int
} deriving (Show, Eq)

instance Ord NanoBot where
  compare (NanoBot _ r1) (NanoBot _ r2) = compare r1 r2

parseInt str = read str :: Int