module AOC.Day09.MarbleGame where

import Data.List.PointedList.Circular
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Traversable as Traversable

getWinningElfScore :: Int -> Int -> Int
getWinningElfScore players lastMarble = do
  let resultingPlayers = getPlayerListAfterGame players lastMarble
  maximum (Traversable.fmapDefault (score) resultingPlayers)

getPlayerListAfterGame players lastMarble = startGame [0..lastMarble] (fromJust $ fromList (map (toPlayer) [1..players]))

startGame (x:xs) players = playMarbles xs (next players) (singleton x)

playMarbles :: [Int] -> PointedList Player -> PointedList Int -> PointedList Player
playMarbles [] players _ = players
playMarbles (x:xs) players board
  | x `mod` 23 == 0 = do
      let currentPlayer = _focus players
      let back7Board = moveBackN 7 board
      let updatedPlayerScore = (score currentPlayer) + x + (_focus back7Board)
      let updatedBoard = delete' back7Board
      let updatedPlayers = replacePlayer players (Player (nr currentPlayer) updatedPlayerScore)
      playMarbles xs (next updatedPlayers) updatedBoard
  | otherwise = do
    let updatedBoard = insert x $ next board
    playMarbles xs (next players) (updatedBoard)

data Player = Player {
  nr :: Int,
  score :: Int
} deriving (Show, Eq)

toPlayer nr = Player nr 0

replacePlayer players updatedPlayer = insert updatedPlayer (fromJust $ deleteLeft players)
delete' circularList = fromJust (delete circularList)

moveBackN :: Int -> PointedList a -> PointedList a
moveBackN n circularList
  | n == 0 = circularList
  | otherwise = moveBackN (n - 1) (previous circularList)

fromList' cList= fromJust $ fromList cList