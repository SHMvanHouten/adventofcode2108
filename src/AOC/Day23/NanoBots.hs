module AOC.Day23.NanoBots where

import AOC.Util.Coord3D
import Data.List.Split
import qualified Data.List as List

getAmountOfBotsInRadius :: NanoBot -> [NanoBot] -> Int
getAmountOfBotsInRadius bot others = length $ getBotsWithinRadius bot others

getBotsWithinRadius (NanoBot {coord= coordinate, radius = radius}) others=
  filter (\c -> coordinate `distance` c <= radius) $ map (coord) others

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
  let rawCoords = head $ splitOn ">" $ drop 5 raw
  let coords = map (parseInt) $ splitOn "," rawCoords
  Coord3d (coords!!0) (coords!!1) (coords!!2)

data NanoBot = NanoBot {
  coord :: Coord3d,
  radius :: Int
} deriving (Show, Eq)

instance Ord NanoBot where
  compare (NanoBot _ r1) (NanoBot _ r2) = compare r1 r2

parseInt str = read str :: Int