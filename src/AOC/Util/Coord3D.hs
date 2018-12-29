module AOC.Util.Coord3D where

import Data.Sequence

data Coord3d = Coord3d{
  x::Int,
  y::Int,
  z::Int
} deriving (Show, Eq)

stubCoord = Coord3d 0 0 0

distance
  (Coord3d{x= x1, y = y1, z = z1})
  (Coord3d{x = x2, y = y2, z = z2}) = (abs (x1 - x2))
                                     + (abs (y1 - y2))
                                     + (abs (z1 - z2))

getCoordsInRange xrange yrange zrange = allCoordsInRange xrange yrange zrange empty

allCoordsInRange :: [Int] -> [Int] -> [Int] -> Seq Coord3d -> Seq Coord3d
allCoordsInRange _ _ [] coords= coords
allCoordsInRange xrange yrange (z:zs) coords = allCoordsInRange xrange yrange zs (allCoordsPlane xrange yrange z empty)><coords

allCoordsPlane _ [] _ coords = coords
allCoordsPlane xrange (y:ys) z coords = allCoordsPlane xrange ys z coords><(allCoordsLine xrange y z empty)

allCoordsLine [] _ _ coords = coords
allCoordsLine (x:xs) y z coords = allCoordsLine xs y z coords|>(Coord3d x y z)