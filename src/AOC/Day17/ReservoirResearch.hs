module AOC.Day17.ReservoirResearch where

import qualified Data.List.Split as Split
import AOC.Util.Coordinate
import Data.Set (Set, fromList, member, map, union, empty, insert, filter)
import Data.Maybe (fromJust, isNothing)

springLocation = Coordinate 500 0

locateAllWetSand :: Coordinate -> Set Coordinate -> Set Coordinate
locateAllWetSand spring clayCoordinates = do
  let wetSand = trickleDown spring clayCoordinates empty bottomY
  Data.Set.filter (\c -> (y' c) >= topY) wetSand
  where bottomY = maximum $ Data.Set.map (y') clayCoordinates
        topY = minimum $ Data.Set.map (y') clayCoordinates

trickleDown :: Coordinate -> Set Coordinate -> Set Coordinate -> Int -> Set Coordinate
trickleDown origin clayCoordinates wetSand bottomY
  | y' oneDown > bottomY = updatedWetSand
  | oneDown `member` clayCoordinates || oneDown `member` wetSand = flowSideWays origin clayCoordinates updatedWetSand bottomY
  | otherwise = trickleDown oneDown clayCoordinates updatedWetSand bottomY
  where oneDown = moveDown origin
        updatedWetSand = (insert origin wetSand)

flowSideWays :: Coordinate -> Set Coordinate -> Set Coordinate -> Int -> Set Coordinate
flowSideWays origin clayCoordinates wetSand bottomY
  | hitClayLeft && hitClayRight = flowSideWays (moveUp origin) clayCoordinates (insert (moveUp origin) allWetSand) bottomY
  | otherwise = allWetSand
  where (wetSandToRight, hitClayRight) = flowRight origin clayCoordinates bottomY wetSand
        (wetSandToLeft, hitClayLeft) = flowLeft origin clayCoordinates bottomY wetSand
        allWetSand = union wetSandToLeft $ union wetSandToRight wetSand

flowRight origin clayCoordinates bottomY wetSand = flowInDirection (moveRight origin) clayCoordinates moveRight wetSand bottomY
flowLeft origin clayCoordinates bottomY wetSand = flowInDirection (moveLeft origin) clayCoordinates moveLeft wetSand bottomY

flowInDirection :: Coordinate -> Set Coordinate -> Move -> Set Coordinate -> Int -> (Set Coordinate, Bool)
flowInDirection origin clayCoordinates nextMove wetSand bottomY
  | origin `member` clayCoordinates = (wetSand, True)
  | (moveDown origin) `member` clayCoordinates || (moveDown origin `member` wetSand) = flowInDirection (nextMove origin) clayCoordinates nextMove (insert origin wetSand) bottomY
  | otherwise = (trickleDown origin clayCoordinates wetSand bottomY, False)

----------------------------
-- Parse and Draw
----------------------------

parseInput :: String -> Set Coordinate
parseInput rawInput = do
  fromList $ concatMap (toCoordinates) $ Prelude.map (\l -> Split.splitOn ", " l) $ lines rawInput

toCoordinates :: [String] -> [Coordinate]
toCoordinates rawStrings
  | singleWideType == 'y' = parseColumn singleWideValue (toRange $ rawStrings!!1)
  | otherwise = parseLine singleWideValue (toRange $ rawStrings!!1)
  where (singleWideType, singleWideValue) = splitTypeToAmount $ rawStrings!!0

parseColumn y xRange = [ Coordinate x y | x <- xRange]

parseLine x yRange = [Coordinate x y | y <- yRange]

splitTypeToAmount :: String -> (Char, Int)
splitTypeToAmount raw = ((split!!0)!!0, (read (split!!1)::Int))
  where split = Split.splitOn "=" raw

toRange raw = [(read (splitRange!!0) :: Int)..(read (splitRange!!1) :: Int)]
  where rawRange = (Split.splitOn "=" raw)!!1
        splitRange = Split.splitOn ".."rawRange

printMap clayCoordinates wetSand = printLines [(minX - 1)..(maxX + 1)] [minY..maxY] clayCoordinates wetSand
  where minX = minimum $ Data.Set.map (x') clayCoordinates
        maxX = maximum $ Data.Set.map (x') clayCoordinates
        minY = minimum $ Data.Set.map (y') clayCoordinates
        maxY = maximum $ Data.Set.map (y') clayCoordinates

printLines xRange [y] clayCoordinates wetSand = printLine xRange y clayCoordinates wetSand
printLines xRange (y:ys) clayCoordinates wetSand = do
  printLine xRange y clayCoordinates wetSand
  printLines xRange ys clayCoordinates wetSand

printLine xRange y clayCoordinates wetSand = print $ Prelude.map (\x -> toCharRepresentation x y clayCoordinates wetSand) xRange

toCharRepresentation x y clayCoordinates wetSand
  | coordinate `member` clayCoordinates = '#'
  | coordinate `member` wetSand = '-'
  | otherwise = '.'
  where coordinate = Coordinate x y

type Move = Coordinate -> Coordinate

type Y = Int
