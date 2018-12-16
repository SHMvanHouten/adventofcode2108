module AOC.Day15.GoblinSlaying where

import AOC.Util.Coordinate
import qualified Data.Set as Set
import qualified Data.Map as Map



-----------------------
-- PARSE BATTLE CAVE
-----------------------
parseBattleCave rawInput = toCaveLine (lines rawInput) 0 emptyBattleCave

toCaveLine :: [String] -> Int -> BattleCave -> BattleCave
toCaveLine [] _ battleCave = battleCave
toCaveLine (rawLine:rawLines) y battleCave = toCaveLine rawLines (y + 1) (parseChar rawLine 0 y battleCave)

parseChar :: String -> Int -> Int -> BattleCave -> BattleCave
parseChar [] _ _ battleCave = battleCave
parseChar (char:chars) x y battleCave
  | char == '.' = parseChar chars (x + 1) y battleCave
  | char == '#' = parseChar chars (x + 1) y (BattleCave (Set.insert currentCoordinate (walls battleCave)) (elves battleCave) (goblins battleCave))
  | char == 'E' = parseChar chars (x + 1) y (BattleCave (walls battleCave) (Map.insert currentCoordinate (Npc currentCoordinate 200) (elves battleCave)) (goblins battleCave))
  | char == 'G' = parseChar chars (x + 1) y (BattleCave (walls battleCave) (elves battleCave) (Map.insert currentCoordinate (Npc currentCoordinate 200) (goblins battleCave)))
  | otherwise = error ("unknown char" ++ (show char))
  where currentCoordinate = Coordinate x y

data BattleCave =  BattleCave {
  walls :: Set.Set Coordinate,
  elves :: Map.Map Coordinate Elf,
  goblins :: Map.Map Coordinate Goblin
} deriving (Show, Eq)

data Npc = Npc {
  coordinate :: Coordinate,
  hitPoints :: Int
} deriving (Show, Eq)

type Elf = Npc
type Goblin = Npc

emptyBattleCave = BattleCave Set.empty Map.empty Map.empty