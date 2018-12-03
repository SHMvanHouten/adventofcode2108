
import Data.List.Split
import Data.Map

main = do
        contents <- readFile "input.txt"
        let claims = Prelude.map (toClaim) (splitOn "\n" contents)
        let claimedCoordinates = concat (Prelude.map (getCoordinatesForClaim) claims)
        let coordinateByAmountUsed = fromListWith (+) (Prelude.map (toPairWithAmount) claimedCoordinates)
        print (size (Data.Map.filter (>1) coordinateByAmountUsed))

toPairWithAmount :: Coordinate -> (Coordinate, Int)
toPairWithAmount c = (c, 1)

getCoordinatesForClaim :: Claim -> [Coordinate]
getCoordinatesForClaim claim = concat [allCoordinatesOnXAxis (x (coordinate claim)) y (width claim)| y <- (getYRange claim)]

getYRange claim = [(y (coordinate claim))..((y (coordinate claim)) + (height claim - 1))]
allCoordinatesOnXAxis :: Int -> Int -> Int -> [Coordinate]
allCoordinatesOnXAxis x y width = [Coordinate xCoord y | xCoord <- [x..(x+width-1)]]

data Coordinate = Coordinate {
  x::Int,
  y::Int
} deriving (Show, Eq, Ord)

data Claim = Claim {
 coordinate :: Coordinate,
 width::Int,
 height::Int
} deriving (Show)

toClaim :: String -> Claim
toClaim rawString = do
                    let relevantFields = getRelevantFieldsForClaim (words rawString)
                    let coordinates = parseCoordinates (fst relevantFields)
                    let widthAndLength = getWidthLength (snd relevantFields)
                    Claim coordinates (fst widthAndLength) (snd widthAndLength)

getRelevantFieldsForClaim :: [String] -> (String, String)
getRelevantFieldsForClaim parts = (parts!!2, parts!!3)

getWidthLength :: String -> (Int, Int)
getWidthLength rawStr = do
                      let splitString = splitOn "x" rawStr
                      (parseInt (splitString!!0), parseInt (splitString!!1))

parseCoordinates :: String -> Coordinate
parseCoordinates rawStr = do
                          let xAndY = splitOn "," (init rawStr)
                          Coordinate (parseInt (xAndY!!0)) (parseInt (xAndY!!1))

parseInt str = read str :: Int