module AOC.Day13.DeadlyRollercoaster where

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.List as List
import AOC.Util.Coordinate
import AOC.Util.Direction

findFirstCrash :: TrackMap -> [Cart] -> Coordinate
findFirstCrash trackMap carts = moveCartsUntilCrash (List.sort carts) [] trackMap

crashUntilOneLasts :: TrackMap -> [Cart] -> Coordinate
crashUntilOneLasts trackMap carts = moveCartsUntilOneIsLeft (List.sort carts) [] trackMap

moveCartsUntilOneIsLeft :: [Cart] -> [Cart] -> TrackMap -> Coordinate
moveCartsUntilOneIsLeft [cart] [] _ = position cart
moveCartsUntilOneIsLeft [] doneCarts trackmap = moveCartsUntilOneIsLeft (List.sort doneCarts) [] trackmap
moveCartsUntilOneIsLeft (cart:nextCarts) doneCarts trackMap
  | detectCrash movedCart (nextCarts++doneCarts) = moveCartsUntilOneIsLeft (removeCrashedCart movedCart nextCarts) (removeCrashedCart movedCart doneCarts) trackMap
  | otherwise = moveCartsUntilOneIsLeft nextCarts (movedCart:doneCarts) trackMap
  where movedCart = moveCart cart trackMap

moveCartsUntilCrash :: [Cart] -> [Cart] -> TrackMap -> Coordinate
moveCartsUntilCrash [] doneCarts trackmap = moveCartsUntilCrash (List.sort doneCarts) [] trackmap
moveCartsUntilCrash (cart:nextCarts) doneCarts trackMap
  | detectCrash movedCart (nextCarts++doneCarts) = position movedCart
  | otherwise = moveCartsUntilCrash nextCarts (movedCart:doneCarts) trackMap
  where movedCart = moveCart cart trackMap

removeCrashedCart cart otherCarts = filter (\p -> (position cart) /= (position p)) otherCarts

------------------
-- move cart one tick
------------------

detectCrash cart otherCarts = any (\other -> other == (position cart)) $ map (position) otherCarts

moveCart :: Cart -> TrackMap -> Cart
moveCart cart trackMap = do
  let nextLocation = getNextPoint cart
  moveAndTurn cart nextLocation trackMap

moveAndTurn :: Cart -> Coordinate -> TrackMap -> Cart
moveAndTurn cart nextLocation trackMap
  | nextTrackType == Vertical || nextTrackType == Horizontal = Cart nextLocation (direction cart) (nextTurn cart)
  | nextTrackType == BackSlash = performBackslashMove cart nextLocation
  | nextTrackType == SlashForward = performSlashForwardMove cart nextLocation
  | nextTrackType == Crossing = doYourCrossThing cart nextLocation
  where nextTrackType = trackType $ trackMap Map.! nextLocation

doYourCrossThing cart nextLocation
  | move == L = Cart nextLocation (turnLeft $ direction cart) (next move)
  | move == FW = Cart nextLocation (direction cart) (next move)
  | move == R = Cart nextLocation (turnRight $ direction cart) (next move)
  where move = nextTurn cart

performBackslashMove cart nextLocation
  | direction cart == North = Cart nextLocation West (nextTurn cart)
  | direction cart == East = Cart nextLocation South (nextTurn cart)
  | direction cart == South = Cart nextLocation East (nextTurn cart)
  | direction cart == West = Cart nextLocation North (nextTurn cart)

performSlashForwardMove cart nextLocation
  | direction cart == North = Cart nextLocation East (nextTurn cart)
  | direction cart == East = Cart nextLocation North (nextTurn cart)
  | direction cart == South = Cart nextLocation West (nextTurn cart)
  | direction cart == West = Cart nextLocation South (nextTurn cart)

getNextPoint cart
  | cartPointedTo == North = moveUp (position cart)
  | cartPointedTo == South = moveDown (position cart)
  | cartPointedTo == West = moveLeft (position cart)
  | cartPointedTo == East = moveRight (position cart)
  where cartPointedTo = direction cart

------------------
-- Parse raw input
------------------

parseRawInput :: String -> (TrackMap, [Cart])
parseRawInput rawInput = parseTrackAndCart (lines rawInput) 0 Map.empty []

parseTrackAndCart :: [String] -> Int -> TrackMap -> [Cart] -> (TrackMap, [Cart])
parseTrackAndCart [] _ tracks carts = (tracks, carts)
parseTrackAndCart (line:lines) yValue tracks carts = do
  let trackNCart = parseLine (zip [0..] line) yValue tracks carts
  parseTrackAndCart lines (yValue + 1) (fst trackNCart) (snd trackNCart)

parseLine :: [(Int, Char)] -> Int -> TrackMap -> [Cart] -> (TrackMap, [Cart])
parseLine [] _ currentTrackMap carts = (currentTrackMap, carts)
parseLine (xToChar:xs) y currentTrackMap carts
  | snd xToChar == ' ' = parseLine xs y currentTrackMap carts
  | snd xToChar == '|' = parseLine xs y (addTrack coordinate Vertical currentTrackMap) carts
  | snd xToChar == '-' = parseLine xs y (addTrack coordinate Horizontal currentTrackMap) carts
  | snd xToChar == '+' = parseLine xs y (addTrack coordinate Crossing currentTrackMap) carts
  | snd xToChar == '\\' = parseLine xs y (addTrack coordinate BackSlash currentTrackMap) carts
  | snd xToChar == '/' = parseLine xs y (addTrack coordinate SlashForward currentTrackMap) carts
  | snd xToChar == '>' = parseLine xs y (addTrack coordinate Horizontal currentTrackMap) $ (buildCart coordinate East):carts
  | snd xToChar == '<' = parseLine xs y (addTrack coordinate Horizontal currentTrackMap) $ (buildCart coordinate West):carts
  | snd xToChar == '^' = parseLine xs y (addTrack coordinate Vertical currentTrackMap) $ (buildCart coordinate North):carts
  | snd xToChar == 'v' = parseLine xs y (addTrack coordinate Vertical currentTrackMap) $ (buildCart coordinate South):carts
  | otherwise = error ("unknown character: " ++ show xToChar)
  where coordinate = Coordinate (fst xToChar) y

buildCart coordinate orientation = Cart coordinate orientation L

addTrack :: Coordinate -> TrackType -> TrackMap -> TrackMap
addTrack coordinate trackType trackMap = Map.insert coordinate (Track (coordinate) trackType) trackMap

data TrackType = Horizontal | Vertical | Crossing | BackSlash | SlashForward deriving (Show, Eq)

data Turn = L | FW| R  deriving (Enum, Show, Eq, Bounded)
instance CircularEnum Turn

data Track = Track {
  location :: Coordinate,
  trackType :: TrackType
} deriving (Show, Eq)

data Cart = Cart {
  position :: Coordinate,
  direction :: Direction,
  nextTurn :: Turn
} deriving (Show, Eq)

instance Ord Cart where
  compare (Cart position1 _ _ ) (Cart position2 _ _) = compare position1 position2

type TrackMap = Map.Map Coordinate Track

--turnLeft :: Direction -> Direction
--turnLeft direction =

-- / nextchar == - || + => SouthToEast :  /-
--                                        |

--                                        |
-- / nextchar /= - || + => WestToNorth : -/


--                                        |
-- \ nextchar == - || + => NorthToWest  : \-

-- \ nextchar /= - || + => SouthToWest  : -\
--                                         |
