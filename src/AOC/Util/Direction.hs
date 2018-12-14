module AOC.Util.Direction where

data Direction = North | East | South | West deriving (Enum, Show, Bounded, Eq)

class (Enum a, Bounded a, Eq a) => CircularEnum a where
     next :: a -> a
     next a = if a == maxBound then minBound else succ a
     prev :: a -> a
     prev a = if a == minBound then maxBound else pred a

instance CircularEnum Direction

turnLeft :: Direction -> Direction
turnLeft direction = prev direction

turnRight :: Direction -> Direction
turnRight direction = next direction