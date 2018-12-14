module AOC.Util.Coordinate where

data Coordinate = Coordinate {
  x' :: Int,
  y' :: Int
} deriving (Show, Eq)

instance Ord Coordinate where
  compare (Coordinate x1 y1) (Coordinate x2 y2)
    | y1 == y2 = compare x1 x2
    | otherwise = compare y1 y2

moveRight :: Coordinate -> Coordinate
moveRight coordinate = Coordinate (x' coordinate + 1) (y' coordinate)

moveLeft :: Coordinate -> Coordinate
moveLeft coordinate = Coordinate (x' coordinate -1) (y' coordinate)

moveDown :: Coordinate -> Coordinate
moveDown coordinate = Coordinate (x' coordinate) (y' coordinate + 1)

moveUp :: Coordinate -> Coordinate
moveUp coordinate = Coordinate (x' coordinate) (y' coordinate - 1)

getAllCoordinatesBetween :: Int -> Int -> Int -> Int -> [Coordinate]
getAllCoordinatesBetween x1 y1 x2 y2 = do
  concatMap (\y -> toLine y [x1..x2]) [y1..y2]

toLine y xRange= map (\x -> Coordinate x y) xRange
