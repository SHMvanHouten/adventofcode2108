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

moveUpLeft coordinate = Coordinate (x' coordinate -1) (y' coordinate - 1)
moveUpRight coordinate = Coordinate (x' coordinate + 1) (y' coordinate - 1)
moveDownLeft coordinate = Coordinate (x' coordinate - 1) (y' coordinate + 1)
moveDownRight coordinate = Coordinate (x' coordinate + 1) (y' coordinate + 1)

allCoordinatesBetween :: Coordinate -> Coordinate -> [Coordinate]
allCoordinatesBetween start end = getAllCoordinatesBetween (x' start) (y' start) (x' end) (y' end)

getAllCoordinatesBetween :: Int -> Int -> Int -> Int -> [Coordinate]
getAllCoordinatesBetween x1 y1 x2 y2 = do
  concatMap (\y -> toLine y [x1..x2]) [y1..y2]

toLine y xRange= map (\x -> Coordinate x y) xRange

getSurroundingCoordinates :: Coordinate -> [Coordinate]
getSurroundingCoordinates coordinate = [moveUp coordinate, moveLeft coordinate, moveRight coordinate, moveDown coordinate]

getCircleAroundCoordinate :: Coordinate -> [Coordinate]
getCircleAroundCoordinate coordinate = [moveUp coordinate, moveLeft coordinate, moveRight coordinate, moveDown coordinate, moveUpLeft coordinate, moveUpRight coordinate, moveDownLeft coordinate, moveDownRight coordinate]

coordinatesForColumn :: Int -> Int -> Int -> [Coordinate]
coordinatesForColumn x minY maxY = getAllCoordinatesBetween x minY x maxY

getXY coordinate = (x' coordinate, y' coordinate)

