import Data.List
import Data.Maybe
import Data.Map

-- <4184
area_x_min = 0
area_x_max = 500
area_y_min = 0
area_y_max = 500
main = do
  contents <- readFile "inputDay6.txt"
--  contents <- readFile "example.txt"
  let centerPoints = Prelude.map toCenter (Prelude.map toCoordinate (lines contents))
  let assignedCenterPoints = assignArea [area_x_min..area_x_max] [area_y_min..area_y_max] centerPoints
  let filteredCenterPoints = Data.List.sort (Prelude.filter (filterOutTheEdgeCases) assignedCenterPoints)
  print (length (claimedPoints (last filteredCenterPoints)))

filterOutTheEdgeCases :: Center -> Bool
filterOutTheEdgeCases center = Prelude.not (Data.List.any (hasClaimedPointOnEdge) (claimedPoints center))

hasClaimedPointOnEdge :: Coordinate -> Bool
hasClaimedPointOnEdge coordinate = (xCoord coordinate == area_x_min) || (xCoord coordinate == area_x_max) || (yCoord coordinate == area_y_min) || (yCoord coordinate == area_y_max)

toCenter :: Coordinate -> Center
toCenter coordinate = Center coordinate []

assignArea :: [Int] -> [Int] -> [Center] -> [Center]
assignArea xCoords [y] centerPoints = assignLine xCoords y centerPoints
assignArea xCoords (y:ys) centerPoints = assignArea xCoords ys (assignLine xCoords y centerPoints)

assignLine :: [Int] -> Int -> [Center] -> [Center]
assignLine [] _ centerPoints = centerPoints
assignLine (x:xs) y centerPoints = assignLine xs y (findClosestPointAndAddCoordinates x y centerPoints)

findClosestPointAndAddCoordinates :: Int -> Int -> [Center] -> [Center]
findClosestPointAndAddCoordinates x y centerPoints
  | isNothing closestCenterToPoint = centerPoints
  | otherwise = addCoordinatesToPoint centerPoints (fromJust closestCenterToPoint) (Coordinate x y)
  where closestCenterToPoint = findClosestCenterToPoint x y centerPoints []

addCoordinatesToPoint :: [Center] -> Center -> Coordinate -> [Center]
addCoordinatesToPoint centerPoints centerToAddTo coordinateToAdd = do
  let locationsToCenters = fromList (Prelude.map (\x -> ((startingPoint x), x)) centerPoints)
  let newClaimedPoints = coordinateToAdd:(claimedPoints centerToAddTo)
  let updatedCenter = Center (startingPoint centerToAddTo) newClaimedPoints
  let updateLocationsToCenters = Data.Map.insert (startingPoint updatedCenter) updatedCenter locationsToCenters
  Data.Map.elems updateLocationsToCenters


findClosestCenterToPoint :: Int -> Int -> [Center] -> [Center] -> Maybe Center
-- We've run through our list and can test if we have a tie
findClosestCenterToPoint x y [] closestPoints
  | (length closestPoints) > 1 = Nothing
  | otherwise = Just (head closestPoints)
findClosestCenterToPoint x y (c:cs) [] = findClosestCenterToPoint x y cs [c]

findClosestCenterToPoint x y (c:cs) closestPoints
  | (calculateDistance c) == (calculateDistance (head closestPoints)) = findClosestCenterToPoint x y cs (c:closestPoints)
  | (calculateDistance c) < (calculateDistance (head closestPoints)) = findClosestCenterToPoint x y cs [c]
  | otherwise = findClosestCenterToPoint x y cs closestPoints
  where calculateDistance = calculateDistanceFromCoordinate x y

calculateDistanceFromCoordinate :: Int -> Int -> Center -> Int
-- we know all centerpoints are in the positive ranges
calculateDistanceFromCoordinate x y centerPoint = do
  let coordinate = startingPoint centerPoint
  abs ((xCoord coordinate) - x) + abs ((yCoord coordinate) - y)

data Center = Center {
  startingPoint :: Coordinate,
  claimedPoints :: [Coordinate]
} deriving (Show, Eq)

data Coordinate = Coordinate {
  xCoord::Int,
  yCoord::Int
} deriving (Show, Eq)

instance Ord Center where
  compare (Center _ points1) (Center _ points2) = compare (length points1) (length points2)

instance Ord Coordinate where
    compare (Coordinate x1 y1) (Coordinate x2 y2) = compare (x1 + y1) (x2 + y2)


-------------------
-- Parsing raw input
-------------------
toCoordinate :: String -> Coordinate
toCoordinate raw = do
  let splitString = words raw
  Coordinate (parseInt (init (splitString!!0))) (parseInt (splitString!!1))

parseInt str = read str :: Int