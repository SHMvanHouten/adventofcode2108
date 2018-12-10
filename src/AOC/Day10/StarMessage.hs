module AOC.Day10.StarMessage where

import Data.List.Split

-- print points: assert all points are within desired range, then draw line for line
-- draw points: foreach coordinate x y , if starlist contains it: X, otherwise .

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

parseInt str = read str :: Int