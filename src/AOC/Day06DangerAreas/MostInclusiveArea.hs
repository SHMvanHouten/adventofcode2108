area_x_min = 0
area_x_max = 500
area_y_min = 0
area_y_max = 500
max_distance = 10000
main = do
  contents <- readFile "inputDay6.txt"
--  contents <- readFile "example.txt"
  let centerPoints = Prelude.map toCoordinate (lines contents)
  let coordinateRange = getAllCoordinatesInRange [area_x_min..area_x_max] [area_y_min..area_y_max] []
  let safePointsAmount = findAllSafePoints coordinateRange centerPoints 0
  print safePointsAmount

findAllSafePoints :: [Coordinate] -> [Coordinate] -> Int -> Int
findAllSafePoints [] centerPoints amountOfSafePoints = amountOfSafePoints
findAllSafePoints (x:xs) centerPoints amountOfSafePoints
  | hasAllCenterPointsInSafeDistance x centerPoints = findAllSafePoints xs centerPoints (amountOfSafePoints + 1)
  | otherwise = findAllSafePoints xs centerPoints amountOfSafePoints

hasAllCenterPointsInSafeDistance :: Coordinate -> [Coordinate] -> Bool
hasAllCenterPointsInSafeDistance x centerPoints = sum (map (\y -> calculateDistanceFromCoordinate x y) centerPoints) < max_distance

calculateDistanceFromCoordinate :: Coordinate -> Coordinate -> Int
-- we know all centerpoints are in the positive ranges
calculateDistanceFromCoordinate first second = do
  abs ((xCoord first) - (xCoord second)) + abs ((yCoord first) - (yCoord second))

data Coordinate = Coordinate {
  xCoord::Int,
  yCoord::Int
} deriving (Show, Eq)

getAllCoordinatesInRange :: [Int] -> [Int] -> [Coordinate] -> [Coordinate]
getAllCoordinatesInRange xCoords [y] coordinates = coordinates ++ [Coordinate x y | x <- xCoords]
getAllCoordinatesInRange xCoords (y:ys) coordinates = getAllCoordinatesInRange xCoords ys (coordinates ++ [Coordinate x y | x <- xCoords])

-------------------
-- Parsing raw input
-------------------
toCoordinate :: String -> Coordinate
toCoordinate raw = do
  let splitString = words raw
  Coordinate (parseInt (init (splitString!!0))) (parseInt (splitString!!1))

parseInt str = read str :: Int
