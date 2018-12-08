import Data.List.Split
import Data.Map
import Data.Set

main = do
        contents <- readFile "input.txt"
        let claims = Prelude.map (toClaim) (lines contents)
        let claimedCoordinates = concat (Prelude.map (getCoordinatesForClaim) claims)
        let coordinateByAmountUsed = fromListWith (+) (zip claimedCoordinates [1,1..])
        let singleUseCoordinates = keys (Data.Map.filter (==1) coordinateByAmountUsed)
        let matcher = allCoordsMatch (Data.Set.fromList singleUseCoordinates)
        let theOneClaim = Prelude.filter(matcher) claims
        print (identity (head theOneClaim))

allCoordsMatch :: Set Coordinate -> Claim -> Bool

allCoordsMatch coordinates claim = allMatch (getCoordinatesForClaim claim) coordinates

allMatch :: [Coordinate] -> Set Coordinate -> Bool
allMatch [x] singleUseCoordinates = x `Data.Set.member` singleUseCoordinates
allMatch (x:xs) singleUseCoordinates
                                  | x `Data.Set.member` singleUseCoordinates = allMatch xs singleUseCoordinates
                                  | otherwise = False

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
 height::Int,
 identity::Int
} deriving (Show)

toClaim :: String -> Claim
toClaim rawString = do
                    let relevantFields = words rawString
                    let coordinates = parseCoordinates (relevantFields!!2)
                    let widthAndLength = getWidthLength (relevantFields!!3)
                    Claim coordinates (fst widthAndLength) (snd widthAndLength) (parseInt (tail (relevantFields!!0)))

getWidthLength :: String -> (Int, Int)
getWidthLength rawStr = do
                      let splitString = splitOn "x" rawStr
                      (parseInt (splitString!!0), parseInt (splitString!!1))

parseCoordinates :: String -> Coordinate
parseCoordinates rawStr = do
                          let xAndY = splitOn "," (init rawStr)
                          Coordinate (parseInt (xAndY!!0)) (parseInt (xAndY!!1))

parseInt str = read str :: Int

