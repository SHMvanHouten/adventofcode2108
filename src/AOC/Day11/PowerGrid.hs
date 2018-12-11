module AOC.Day11.PowerGrid where

import AOC.Util.Coordinate
import qualified Data.Char as Char
import qualified Data.Map as Map

findTopLeftOfHighestPowerSquare :: Int -> Coordinate
findTopLeftOfHighestPowerSquare gridNr = do
  let grid = buildGrid
  let powerCells = Map.fromList $ map (\x -> (coordinate x, x)) $ map (\c -> toPowerCell c gridNr) grid
  let coordinatesToCheck = getAllCoordinatesBetween 0 0 (300 - 3) (300 - 3)
  let threeByThrees = map (\x -> getThreeByThreeOfTopLeftCoordinate x powerCells) coordinatesToCheck

  topLeftCoordinate $ maximum threeByThrees

getThreeByThreeOfTopLeftCoordinate :: Coordinate -> Map.Map Coordinate PowerCell -> ThreeByThree
getThreeByThreeOfTopLeftCoordinate coordinate powerCells = ThreeByThree coordinate (getThreeByThreePower coordinate powerCells)

getThreeByThreePower :: Coordinate -> Map.Map Coordinate PowerCell -> Int
getThreeByThreePower coordinate powerCells = do
  let grid = getAllCoordinatesBetween (x' coordinate) (y' coordinate) (x' coordinate + 2) (y' coordinate + 2)
  sum $ map (\c -> getCoordinatesPower c powerCells) grid

getCoordinatesPower :: Coordinate -> Map.Map Coordinate PowerCell -> Int
getCoordinatesPower coordinate powerCells = power $ powerCells Map.! coordinate

buildGrid = getAllCoordinatesBetween 0 0 300 300

toPowerCell coordinate gridNr = PowerCell coordinate (getPowerLevel coordinate gridNr)

---------------
-- GetPowerLevel
---------------
getPowerLevel coordinate gridNr = do
  let rackId = getRackId coordinate
  let tempPower = ((rackId * (y' coordinate)) + gridNr) * rackId
  (getHundredValueDigit tempPower) - 5

getHundredValueDigit int
  | length str <= 2 = 0
  | otherwise = Char.digitToInt $ str!!(length str - 3)
  where str = show int

getRackId :: Coordinate -> Int
getRackId coordinate = (x' coordinate) + 10

data PowerCell = PowerCell {
  coordinate :: Coordinate,
  power :: Int
} deriving (Show, Eq)

data ThreeByThree = ThreeByThree {
  topLeftCoordinate :: Coordinate,
  totalPower :: Int
} deriving (Show, Eq)

instance Ord ThreeByThree where
    compare (ThreeByThree c1 totalPower1) (ThreeByThree c2 totalPower2) = compare totalPower1 totalPower2