module AOC.Util.SequenceHelper where

import Data.Sequence

-- fails on empty sequences
lastElem :: Seq a -> a
lastElem (viewr -> xs :> x) = x

firstElemAndRest (viewl -> x :< xs) = (x, xs)
firstElem (viewl -> x :< xs) = x

anyMatch :: (Ord a) => a -> Seq a -> Bool
anyMatch y (x:<|xs)
  | y == x = True
  | otherwise = anyMatch y xs
anyMatch _ empty = False
