module AOC.Util.Coordinate where

data Coordinate = Coordinate {
  x' :: Int,
  y' :: Int
} deriving (Show, Eq, Ord)

getAllCoordinatesBetween :: Int -> Int -> Int -> Int -> [Coordinate]
getAllCoordinatesBetween x1 y1 x2 y2 = do
  concatMap (\y -> toLine y [x1..x2]) [y1..y2]

toLine y xRange= map (\x -> Coordinate x y) xRange
