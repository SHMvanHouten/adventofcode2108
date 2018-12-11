module AOC.Day10.StarMessage where

import Data.List.Split
import qualified Data.Set as Set
import qualified Data.List as List

-- 10515

tickNTimes :: [Star] -> Int -> [Star]
tickNTimes stars n
  | n == 0 = stars
  | otherwise = tickNTimes (tick stars) (n - 1)

moveStarsUntilMinimumWidth :: [Star] -> Int -> Int -> [Star] -> ([Star], Int)
moveStarsUntilMinimumWidth stars i prevWidth prevStars
  | currentWidth > prevWidth = (prevStars, i)
  | otherwise = moveStarsUntilMinimumWidth (tick stars) (i + 1) currentWidth stars
  where currentWidth = getWidth stars

getWidth stars = length $ getXRange stars

tick :: [Star] -> [Star]
tick stars = map (moveStarOneTick) stars

moveStarOneTick star = do
  let newX = (getX star) + (getXVelocity star)
  let newY = (getY star) + (getYVelocity star)
  Star (Coordinate newX newY) (velocity star)

drawSky :: [Star] -> String
drawSky stars = do
  let starLocation = Set.fromList $ map (coordinate) stars
  concat $ List.intersperse "," $ map (\y -> printLine y starLocation (getXRange stars)) (getYRange stars)

printLine :: Int -> Set.Set Coordinate -> [Int] -> String
printLine y starLocations xRange = map (\x -> starOrVoid x y starLocations) xRange

starOrVoid x y starLocations
  | (Coordinate x y) `Set.member` starLocations = 'X'
  | otherwise = '.'

parseStars :: String -> [Star]
parseStars rawInput = map parseStar $ lines rawInput

parseStar :: String -> Star
parseStar rawStar = do
  let splitStar = splitOn "<" rawStar
  let coordinate = parseCoordinate (splitStar!!1)
  let velocity = parseVelocity (splitStar!!2)
  Star coordinate velocity

parseVelocity rawVelocity = do
  let splitVel = splitOn "," rawVelocity
  let xv = parseInt $ removeSpace $ splitVel!!0
  let yv = parseInt $ removeTrailing $ removeSpace $ splitVel!!1
  Velocity xv yv

parseCoordinate rawCoordinate = do
  let splitCoords = splitOn "," rawCoordinate
  let x = parseInt $ removeSpace $ splitCoords!!0
  let y = parseInt $ removeTrailing $ removeSpace $ splitCoords!!1
  Coordinate x y

removeSpace str
  | head str == ' ' = tail str
  | otherwise = str

removeTrailing str = head $ splitOn ">" str

data Coordinate = Coordinate {
  x' :: Int,
  y' :: Int
} deriving (Show, Eq, Ord)

data Velocity = Velocity {
  xv :: Int,
  yv :: Int
} deriving (Show, Eq)

data Star = Star {
  coordinate :: Coordinate,
  velocity :: Velocity
} deriving (Show, Eq)

getX star = x' $ coordinate star
getY star = y' $ coordinate star
getXVelocity star = xv $ velocity star
getYVelocity star = yv $ velocity star

getXRange stars = do
  let allXs = List.sort $ map (x') $ map (coordinate) stars
  [(head allXs)..(last allXs)]

getYRange stars = do
  let allYs = List.sort $ map (y') $ map (coordinate) stars
  [(head allYs)..(last allYs)]

parseInt str = read str :: Int