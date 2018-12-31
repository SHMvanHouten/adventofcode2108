module AOC.Util.Coord4D where

import Data.Sequence

data Coord4d = Coord4d{
  x::Int,
  y::Int,
  z::Int,
  q::Int
} deriving (Show, Eq)

stubCoord = Coord4d 0 0 0 0

distance
  (Coord4d{x= x1, y = y1, z = z1, q = q1})
  (Coord4d{x = x2, y = y2, z = z2, q = q2}) = (abs (x1 - x2))
                                     + (abs (y1 - y2))
                                     + (abs (z1 - z2))
                                     + (abs) (q1 - q2)
