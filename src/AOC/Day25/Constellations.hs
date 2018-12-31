module AOC.Day25.Constellations where

import AOC.Util.Coord4D
import Data.List.Split
import Data.List

assembleConstellations :: [Coord4d] -> [Constellation] -> [Constellation]
assembleConstellations [] constellations = constellations
assembleConstellations (c:cs) constellations = do
  let (remainingPoints, constellation) = buildConstellation c cs []
  assembleConstellations remainingPoints (constellation:constellations)

buildConstellation :: Coord4d -> [Coord4d] -> Constellation -> ([Coord4d], Constellation)
buildConstellation coord others constellationSoFar
  | Prelude.length pointsInRange == 0 = (rest, constellation)
  | otherwise = addToConstellation pointsInRange rest constellation
  where (pointsInRange, rest) = partition (\c -> distance c coord <= 3) others
        constellation = coord:constellationSoFar

addToConstellation [] rest constellation = (rest, constellation)
addToConstellation (p:ps) rest constellation = do
  let (newRest, expandedConstellation) = buildConstellation p rest constellation
  addToConstellation (ps \\ expandedConstellation) newRest expandedConstellation

----------
-- parse
----------

type Constellation = [Coord4d]

parseInput raw = do
  let split = lines raw
  map (parse4dCoord) split

parse4dCoord raw = do
  let coords = map (parseInt) $ splitOn (",") raw
  Coord4d (coords!!0) (coords!!1) (coords!!2) (coords!!3)

parseInt str = read str :: Int