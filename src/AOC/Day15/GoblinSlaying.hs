module AOC.Day15.GoblinSlaying where

import AOC.Util.Coordinate
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.List as List
import Data.Traversable (fmapDefault)
import Data.Maybe

-- takeStep
-- remove elf from battlecave
-- do the elves move
-- re-add the elf to the battlecave
-- attackPower = 3
attackPower = 3

doTurn :: BattleCave -> BattleCave
doTurn battleCave = do
  let orderedNpcCoordinates = orderNpcs (elves battleCave) (goblins battleCave)
  doNpcTurns orderedNpcCoordinates battleCave

doNpcTurns :: [Coordinate] -> BattleCave -> BattleCave
doNpcTurns [] battleCave = battleCave
doNpcTurns (currentNpcCoordinate:otherNpcs) battleCave = do
  let (currentNpc, battleCaveWithoutNpc) = getAndRemoveCurrentNpcFromBattleCave currentNpcCoordinate battleCave
  let (battleWasDone, battleCaveAfterBattle) = attackAdjacentEnemy currentNpc battleCaveWithoutNpc
  if battleWasDone then doNpcTurns otherNpcs (addNpcToBattleCave currentNpc battleCaveAfterBattle)
  else do
    let updatedNpc = moveNpc currentNpc battleCaveWithoutNpc
    let updatedBattleCave = addNpcToBattleCave updatedNpc battleCaveWithoutNpc
    doNpcTurns otherNpcs updatedBattleCave

attackAdjacentEnemy :: Npc -> BattleCave -> (BattleWasDone, BattleCave)
attackAdjacentEnemy npc battleCave
  | species npc == Elves = do
    let enemies = goblins battleCave
    let (battleWasDone, updatedEnemies) = attackWeakestEnemyNextToNpc npc enemies
    if battleWasDone then (True, BattleCave (walls battleCave) (elves battleCave) updatedEnemies)
    else (False, battleCave)
  | species npc == Goblins = do
    let enemies = elves battleCave
    let (battleWasDone, updatedEnemies) = attackWeakestEnemyNextToNpc npc enemies
    if battleWasDone then (True, BattleCave (walls battleCave) updatedEnemies (goblins battleCave))
    else (False, battleCave)

attackWeakestEnemyNextToNpc:: Npc -> Map.Map Coordinate Npc -> (BattleWasDone, Map.Map Coordinate Npc)
attackWeakestEnemyNextToNpc npc enemies = do
    let weakestEnemy = findWeakestAdjacentEnemy npc enemies
    if weakestEnemy == Nothing
    then (False, enemies)
    else do
      let updatedWeakestEnemy = doDamageToEnemy $ fromJust weakestEnemy
      let updatedEnemies = Map.insert (coordinate updatedWeakestEnemy) updatedWeakestEnemy enemies
      (True, updatedEnemies)

findWeakestAdjacentEnemy :: Npc -> Map.Map Coordinate Npc -> Maybe Npc
findWeakestAdjacentEnemy npc enemies = do
  let adjacentEnemyCoordinates = filter (\c -> c `Map.member` enemies) (getSurroundingCoordinates (coordinate npc))
  if List.length adjacentEnemyCoordinates > 0
  then do
    let adjacentEnemies = map (\c -> enemies Map.! c) adjacentEnemyCoordinates
    Just $ head $ List.sort adjacentEnemies
  else
    Nothing

doDamageToEnemy enemy = Npc (coordinate enemy) (hitPoints enemy - attackPower) (species enemy)

addNpcToBattleCave npc battleCave
  | species npc == Elves = BattleCave (walls battleCave) (Map.insert (coordinate npc) npc (elves battleCave)) (goblins battleCave)
  | species npc == Goblins = BattleCave (walls battleCave) (elves battleCave) (Map.insert (coordinate npc) npc (goblins battleCave))

getAndRemoveCurrentNpcFromBattleCave :: Coordinate -> BattleCave -> (Npc, BattleCave)
getAndRemoveCurrentNpcFromBattleCave coordinate battleCave
  | coordinate `Map.member` (goblins battleCave) = (((goblins battleCave) Map.! coordinate), BattleCave (walls battleCave) (elves battleCave) (Map.delete coordinate (goblins battleCave)))
  | coordinate `Map.member` (elves battleCave) = (((elves battleCave) Map.! coordinate), BattleCave (walls battleCave) (Map.delete coordinate (elves battleCave)) (goblins battleCave))
  | otherwise = error "coordinate not found in elves or goblins!"

orderNpcs :: Map.Map Coordinate Elf -> Map.Map Coordinate Goblin -> [Coordinate]
orderNpcs elves goblins = List.sort ((Map.keys elves) ++ (Map.keys goblins))

moveNpc :: Npc -> BattleCave -> Npc
moveNpc npc battleCave = do
  if species npc == Elves
    then do
      let shortestPath = findShortestPathToOpponent npc (Set.union (walls battleCave) (Set.fromList (Map.keys (elves battleCave)))) (Set.fromList $ Map.keys $ goblins battleCave)
      Npc (takeSecond (pathSoFar shortestPath)) (hitPoints npc) (species npc)
    else do
      let shortestPath = findShortestPathToOpponent npc (Set.union (walls battleCave) (Set.fromList (Map.keys (goblins battleCave)))) (Set.fromList $ Map.keys $ elves battleCave)
      Npc (takeSecond (pathSoFar shortestPath)) (hitPoints npc) (species npc)

findShortestPathToOpponent :: Npc -> Set.Set Coordinate -> Set.Set Coordinate -> Path
findShortestPathToOpponent npc obstacles opponents = findPaths (Seq.fromList [Path npcCoordinate (Seq.fromList [npcCoordinate])]) obstacles opponents
  where npcCoordinate = coordinate npc

-- todo: change return value path to maybe in case all opponents are dead? or check for empty elves and goblins beforehand
findPaths :: Seq.Seq Path -> Set.Set Coordinate -> Set.Set Coordinate -> Path
findPaths paths obstacles opponents
  | isAdjacentToOpponent currentPath opponents = currentPath
  | otherwise = findPaths (addNextStepsOfCurrentPath currentPath obstacles restOfPaths) obstacles opponents
  where (currentPath, restOfPaths) = splitFirstAndRest paths

addNextStepsOfCurrentPath :: Path -> Set.Set Coordinate -> Seq.Seq Path -> Seq.Seq Path
addNextStepsOfCurrentPath currentPath obstacles paths = do
  let freeCoordinates = findFreeCoordinates currentPath obstacles paths
  let pathsToAdd = Seq.fromList $ map (\c -> toPath c currentPath) freeCoordinates
  paths Seq.>< pathsToAdd

findFreeCoordinates :: Path -> Set.Set Coordinate -> Seq.Seq Path -> [Coordinate]
findFreeCoordinates currentPath obstacles otherPaths = do
  let surroundingCoords = removeLastCoordinateOfCurrentPath (pathSoFar currentPath) (getSurroundingCoordinates (currentPosition currentPath))
  let coordsNotAlreadyPathedTo = filter (\c -> coordIsNotAlreadyPathedTo c (fmapDefault (currentPosition) otherPaths)) surroundingCoords
  filter (\c -> c `notElem` obstacles) surroundingCoords

coordIsNotAlreadyPathedTo coordinate takenCoordinates = not (any (\c -> c == coordinate) takenCoordinates)

removeLastCoordinateOfCurrentPath :: Seq.Seq Coordinate -> [Coordinate] -> [Coordinate]
removeLastCoordinateOfCurrentPath pathsSoFar coordinates
  | lastCoordinate == Nothing = coordinates
  | otherwise = coordinates List.\\ [fromJust lastCoordinate]
  where lastCoordinate = getLast pathsSoFar

toPath coordinate oldPath = Path coordinate ((pathSoFar oldPath) Seq.|> coordinate)

isAdjacentToOpponent :: Path -> Set.Set Coordinate -> Bool
isAdjacentToOpponent path opponents = any (\c -> c `Set.member` opponents) $ getSurroundingCoordinates (currentPosition path)

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
  | char == 'E' = parseChar chars (x + 1) y (BattleCave (walls battleCave) (Map.insert currentCoordinate (Npc currentCoordinate 200 Elves) (elves battleCave)) (goblins battleCave))
  | char == 'G' = parseChar chars (x + 1) y (BattleCave (walls battleCave) (elves battleCave) (Map.insert currentCoordinate (Npc currentCoordinate 200 Goblins) (goblins battleCave)))
  | otherwise = error ("unknown char" ++ (show char))
  where currentCoordinate = Coordinate x y

data BattleCave =  BattleCave {
  walls :: Set.Set Coordinate,
  elves :: Map.Map Coordinate Elf,
  goblins :: Map.Map Coordinate Goblin
} deriving (Show, Eq)

data Npc = Npc {
  coordinate :: Coordinate,
  hitPoints :: Int,
  species :: Species
} deriving (Show, Eq)

instance Ord Npc where
  compare (Npc c1 hp1 _) (Npc c2 hp2 _)
    | hp1 == hp2 = compare c1 c2
    | otherwise = compare hp1 hp2

--instance Ord Coordinate where
--  compare (Coordinate x1 y1) (Coordinate x2 y2)
--    | y1 == y2 = compare x1 x2
--    | otherwise = compare y1 y2

type Elf = Npc
type Goblin = Npc

data Path = Path {
  currentPosition :: Coordinate,
  pathSoFar :: Seq.Seq Coordinate
}

data Species = Elves | Goblins deriving (Show, Eq)

emptyBattleCave = BattleCave Set.empty Map.empty Map.empty

splitFirstAndRest :: Seq.Seq a -> (a, Seq.Seq a)
splitFirstAndRest seq = (takeFirst seq, Seq.drop 1 seq)

takeFirst :: Seq.Seq a -> a
takeFirst seq = fromJust $ seq Seq.!? 0

takeSecond :: Seq.Seq a -> a
takeSecond seq = fromJust $ seq Seq.!? 1

getLast :: Seq.Seq a -> Maybe a
getLast (Seq.viewr -> xs Seq.:> x) = Just x

type BattleWasDone = Bool