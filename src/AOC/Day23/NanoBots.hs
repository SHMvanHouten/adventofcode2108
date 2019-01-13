module AOC.Day23.NanoBots where

import AOC.Util.Coord3D
import Data.List.Split
import qualified Data.List as List
import Data.Sequence
import qualified Data.Set as Set
import AOC.Day23.SphereCubeCollisionDetect
import AOC.Day23.Types

challengeFindPointInRangeOfMostBots bots = do
  let bestBox = divideIntoBoxesUntilSinglePoint [BoxWithBots (Box (Coord3d 30484500 13948900 38118800) (Coord3d 50484500 33948900 58118800)) (Set.fromList bots)]
  minCoord $ box $ bestBox

findPointInRangeOfMostBots bots = do
  let bestBox = divideIntoBoxesUntilSinglePoint [BoxWithBots (determineBox bots) (Set.fromList bots)]
  minCoord $ box $ bestBox

divideIntoBoxesUntilSinglePoint :: [BoxWithBots] -> BoxWithBots
divideIntoBoxesUntilSinglePoint boxes
  | isSinglePoint $ box $ head boxes = head boxes
  | otherwise = divideIntoBoxesUntilSinglePoint (findBestBox boxes [])

isSinglePoint (Box min max) = min == max

findBestBox :: [BoxWithBots] -> [BoxWithBots] -> [BoxWithBots]
findBestBox [] bestDividedBoxes = bestDividedBoxes
findBestBox (boxWithBots:boxesWithBots) bestDividedBoxes = do
  let dividedBoxes = divideBoxInto8 $ box boxWithBots
  findBestBox boxesWithBots (findBestDividedBoxes dividedBoxes (bots boxWithBots) bestDividedBoxes)

findBestDividedBoxes :: [Box] -> Set.Set NanoBot -> [BoxWithBots] -> [BoxWithBots]
findBestDividedBoxes [] _ best = best
findBestDividedBoxes (box:boxes) nanoBots bestSoFar
  | List.null bestSoFar || List.length botsCollidingWithBox > (List.length $ bots $ head bestSoFar) = findBestDividedBoxes boxes nanoBots [BoxWithBots box botsCollidingWithBox]
  | List.length botsCollidingWithBox == (List.length $ bots $ head bestSoFar) = findBestDividedBoxes boxes nanoBots ((BoxWithBots box botsCollidingWithBox):bestSoFar)
  | otherwise = findBestDividedBoxes boxes nanoBots bestSoFar
  where botsCollidingWithBox = findBotsThatCollideWithBox box nanoBots

-- overlapping boxes is fine, it means they are all the same size
divideBoxInto8 (Box (Coord3d minx miny minz) (Coord3d maxx maxy maxz)) = do
  let (halfwayX, halfwayXUp) = halveAndRoundUp minx maxx
  let (halfwayY, halfwayYUp) = halveAndRoundUp miny maxy
  let (halfwayZ, halfwayZUp) = halveAndRoundUp minz maxz
  [ Box (Coord3d minx miny minz)         (Coord3d halfwayXUp halfwayYUp halfwayZUp),
    Box (Coord3d minx halfwayY minz)     (Coord3d halfwayXUp maxy halfwayZUp),
    Box (Coord3d minx miny halfwayZ)     (Coord3d halfwayXUp halfwayYUp maxz),
    Box (Coord3d minx halfwayY halfwayZ) (Coord3d halfwayXUp maxy maxz),
    
    Box (Coord3d halfwayX miny minz)     (Coord3d maxx halfwayYUp halfwayZUp),
    Box (Coord3d halfwayX halfwayY minz) (Coord3d maxx maxy halfwayZUp),
    Box (Coord3d halfwayX miny halfwayZ) (Coord3d maxx halfwayYUp maxz),
    Box (Coord3d halfwayX halfwayY halfwayZ) (Coord3d maxx maxy maxz)
    ]

determineBox bots = do
  let minx = minimum $ map (x) $ map (coord) bots
  let maxx = maximum $ map (x) $ map (coord) bots
  let miny = minimum $ map (y) $ map (coord) bots
  let maxy = maximum $ map (y) $ map (coord) bots
  let minz = minimum $ map (z) $ map (coord) bots
  let maxz = maximum $ map (z) $ map (coord) bots
  Box (Coord3d minx miny minz) (Coord3d maxx maxy maxz)

halveAndRoundUp minA maxA
  | maxA-minA == 1 = (maxA, minA)
  | otherwise = (((minA + maxA) `div` 2), (minA + maxA) `divRoundUp` 2)










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

parseInt str = read str :: Int

coordsInBotRange bots = do
  let botCoords = map (coord) bots
  let minX = minimum $ map (x) botCoords
  let maxX = maximum $ map (x) botCoords
  let minY = minimum $ map (y) botCoords
  let maxY = maximum $ map (y) botCoords
  let minZ = minimum $ map (z) botCoords
  let maxZ = maximum $ map (z) botCoords
  getCoordsInRange [minX..maxX] [minY..maxY] [minZ..maxZ]

--
--efficientFindPoint bots i coordinateRange
--  | i == 0 = bestPoint
--  | otherwise = efficientFindPoint bots (i - 1) (extrapolateBestPoint bestPoint)
--  where dividedBots = map (`divideBot` (10^i)) bots
--        bestPoint = findPointWithMostBots coordinateRange dividedBots (stubCoord, 0)
--
--extrapolateBestPoint coord = do
--  let (Coord3d x y z) = coord `times` 10
--  getCoordsInRange [(x - 10)..(x + 10)] [(y - 10)..(y + 10)] [(z - 10)..(z + 10)]
--
divideBot (NanoBot coord r) divisor = NanoBot (coord `divide` divisor) (r `div` divisor)