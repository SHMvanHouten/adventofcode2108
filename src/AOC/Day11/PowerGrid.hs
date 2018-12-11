module AOC.Day11.PowerGrid where

import AOC.Util.Coordinate
import qualified Data.Char as Char
import qualified Data.Map as Map

findTopLeftOfHighestPowerSquareOfAnySize :: Int -> XByX
findTopLeftOfHighestPowerSquareOfAnySize gridNr = do
  let grid = buildGrid
  let powerCells = Map.fromList $ map (\x -> (coordinate x, x)) $ map (\c -> toPowerCell c gridNr) grid
  let xByXes = concatMap (\x -> findPowerSquaresForSquareSize powerCells x) [2..20]
  maximum xByXes

findTopLeftOfHighestPowerSquare :: Int -> Coordinate
findTopLeftOfHighestPowerSquare gridNr = do
  let grid = buildGrid
  let powerCells = Map.fromList $ map (\x -> (coordinate x, x)) $ map (\c -> toPowerCell c gridNr) grid
  let threeByThrees = findPowerSquaresForSquareSize powerCells 3
  topLCoordinate $ maximum threeByThrees

findPowerSquaresForSquareSize :: Map.Map Coordinate PowerCell -> Int -> [XByX]
findPowerSquaresForSquareSize powerCells squareSize = do
  let coordinatesToCheck = getAllCoordinatesBetween 0 0 (300 - squareSize) (300 - squareSize)
  map (\c -> getXByXOfTopLeftCoordinate c squareSize powerCells) coordinatesToCheck

getXByXOfTopLeftCoordinate :: Coordinate -> Int -> Map.Map Coordinate PowerCell -> XByX
getXByXOfTopLeftCoordinate coordinate gridSize powerCells = XByX coordinate gridSize (getXByXPower coordinate gridSize powerCells)

getXByXPower :: Coordinate -> Int -> Map.Map Coordinate PowerCell -> Int
getXByXPower coordinate gridSize powerCells = do
  let grid = getAllCoordinatesBetween (x' coordinate) (y' coordinate) (x' coordinate + gridSize - 1) (y' coordinate + gridSize - 1)
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

data XByX = XByX {
  topLCoordinate :: Coordinate,
  size :: Int,
  theTotalPower :: Int
} deriving (Show, Eq)

instance Ord XByX where
    compare (XByX c1 s1 theTotalPower1) (XByX c2 s2 theTotalPower2) = compare theTotalPower1 theTotalPower2