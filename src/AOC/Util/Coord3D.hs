module AOC.Util.Coord3D where

data Coord3d = Coord3d{
  x::Int,
  y::Int,
  z::Int
} deriving (Show, Eq)

distance (Coord3d{x= x1, y = y1, z = z1}) (Coord3d{x = x2, y = y2, z = z2}) = (abs (x1 - x2))
                                                                            + (abs (y1 - y2))
                                                                            + (abs (z1 - z2))