module AOC.Day15.GoblinSlaying where

import AOC.Util.Coordinate
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.List as List
import Data.Traversable (fmapDefault)
import Data.Maybe

printBattleCave battleCave turn = do
    let width = getCaveWidth (walls battleCave)
    let height = getCaveHeight (walls battleCave)
    print turn
    printLines height width battleCave

printLines [] _ _ = print ",   "
printLines (y:ys) width battleCave = do
  print $ buildLine width y battleCave []
  printLines ys width battleCave

buildLine width y battleCave npcs = do
  let gridLine = map (\x -> toChar x y battleCave) width
  let elves = map (\x -> getElf x y battleCave) $ filter (\x -> isElf x y battleCave) width
  let goblins = map (\x -> getGoblin x y battleCave) $ filter (\x -> isGoblin x y battleCave) width
  gridLine ++ " " ++ (show elves) ++ (show goblins)

getElf x y battleCave = (elves battleCave) Map.! (Coordinate x y)

getGoblin x y battleCave = (goblins battleCave) Map.! (Coordinate x y)

isElf x y battleCave = (Map.member coordinate (elves battleCave))
  where coordinate = Coordinate x y

isGoblin x y battleCave = (Map.member coordinate (goblins battleCave))
  where coordinate = Coordinate x y

toChar x y battleCave
  | Set.member coordinate (walls battleCave) = '#'
  | Map.member coordinate (elves battleCave) = 'E'
  | Map.member coordinate (goblins battleCave) = 'G'
  | otherwise = '.'
  where coordinate = Coordinate x y

getCaveWidth walls = do
  let xes = Set.map (x') walls
  [(minimum xes)..(maximum xes)]

getCaveHeight walls = do
  let ys = Set.map (y') walls
  [(minimum ys)..(maximum ys)]

printCaveConflict :: BattleCave -> Int -> IO ()
printCaveConflict battleCave turn = do
  let updatedBattleCave = doTurn battleCave
  if allGoblinsOrElvesWereKilled updatedBattleCave then do
        printBattleCave updatedBattleCave (turn + 1)
  else do
    printBattleCave updatedBattleCave (turn + 1)
    printCaveConflict updatedBattleCave (turn + 1)

findFirstTimeAllElvesSurvive :: String -> Int -> (Int, Int)
findFirstTimeAllElvesSurvive rawInput elfPower = do
  let battleCave = parseBattleCave rawInput elfPower
  let (result, valueOfEndConflict) = resolveCaveConflictUntilElfDies battleCave 0 (Map.size (elves battleCave))
  if result then (valueOfEndConflict, elfPower)
  else findFirstTimeAllElvesSurvive rawInput (elfPower + 1)


resolveCaveConflictUntilElfDies :: BattleCave -> Int -> Int -> (Bool, Int)
resolveCaveConflictUntilElfDies battleCave turns startingElfForce
  | (Map.size (elves battleCave) < startingElfForce) = (False, (calculateEndResult updatedBattleCave turns))
  | Map.size (goblins battleCave) == 0 = (True, (calculateEndResult updatedBattleCave turns))
  | otherwise = resolveCaveConflictUntilElfDies updatedBattleCave (turns + 1) startingElfForce
  where updatedBattleCave = doTurn battleCave

resolveCaveConflict :: BattleCave -> Int -> (Int, Int)
resolveCaveConflict battleCave turns
  | allGoblinsOrElvesWereKilled updatedBattleCave = (turns, (calculateEndResult updatedBattleCave turns))
  | otherwise = resolveCaveConflict updatedBattleCave (turns + 1)
  where updatedBattleCave = doTurn battleCave

calculateEndResult :: BattleCave -> Int -> Int
calculateEndResult battleCave turns = turns * (List.sum $ map(hitPoints) $ Map.elems $ Map.union (elves battleCave) (goblins battleCave))

doTurn :: BattleCave -> BattleCave
doTurn battleCave = do
  let orderedNpcCoordinates = orderNpcs (elves battleCave) (goblins battleCave)
  doNpcTurns orderedNpcCoordinates battleCave

doNpcTurns :: [Coordinate] -> BattleCave -> BattleCave
doNpcTurns [] battleCave = battleCave
doNpcTurns (currentNpcCoordinate:otherNpcs) battleCave = do
  let (maybeNpc, battleCaveWithoutNpc) = getAndRemoveCurrentNpcFromBattleCave currentNpcCoordinate battleCave
  -- npc was killed before its turn:
  if isNothing maybeNpc then doNpcTurns otherNpcs battleCave
  else do
    if allGoblinsOrElvesWereKilled battleCave then battleCave
    else do
      let currentNpc = fromJust maybeNpc
      let (battleWasDone, battleCaveAfterBattle) = attackAdjacentEnemy currentNpc battleCaveWithoutNpc
      if battleWasDone then doNpcTurns otherNpcs (addNpcToBattleCave currentNpc battleCaveAfterBattle)
      else do
        let updatedNpc = moveNpc currentNpc battleCaveWithoutNpc
        let (_, battleCaveAfterBattle) = attackAdjacentEnemy updatedNpc battleCaveWithoutNpc
        let updatedBattleCave = addNpcToBattleCave updatedNpc battleCaveAfterBattle
        doNpcTurns otherNpcs updatedBattleCave

allGoblinsOrElvesWereKilled battleCave = Map.size (elves battleCave) == 0 || Map.size (goblins battleCave) == 0

attackAdjacentEnemy :: Npc -> BattleCave -> (BattleWasDone, BattleCave)
attackAdjacentEnemy npc battleCave
  | species npc == Elf = do
    let enemies = goblins battleCave
    let (battleWasDone, updatedEnemies) = attackWeakestEnemyNextToNpc npc enemies
    if battleWasDone then (True, BattleCave (walls battleCave) (elves battleCave) updatedEnemies)
    else (False, battleCave)
  | species npc == Goblin = do
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
      let updatedWeakestEnemy = doDamageToEnemy (fromJust weakestEnemy) (attackPower npc)
      if hitPoints updatedWeakestEnemy <= 0
      then (True, Map.delete (coordinate updatedWeakestEnemy) enemies)
      else do
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

doDamageToEnemy :: Npc -> Int -> Npc
doDamageToEnemy enemy damage = Npc (coordinate enemy) (hitPoints enemy - damage) (species enemy) (attackPower enemy)

addNpcToBattleCave npc battleCave
  | species npc == Elf = BattleCave (walls battleCave) (Map.insert (coordinate npc) npc (elves battleCave)) (goblins battleCave)
  | species npc == Goblin = BattleCave (walls battleCave) (elves battleCave) (Map.insert (coordinate npc) npc (goblins battleCave))

getAndRemoveCurrentNpcFromBattleCave :: Coordinate -> BattleCave -> (Maybe Npc, BattleCave)
getAndRemoveCurrentNpcFromBattleCave coordinate battleCave
  | coordinate `Map.member` (goblins battleCave) = ((Just $ (goblins battleCave) Map.! coordinate), BattleCave (walls battleCave) (elves battleCave) (Map.delete coordinate (goblins battleCave)))
  | coordinate `Map.member` (elves battleCave) = ((Just $ (elves battleCave) Map.! coordinate), BattleCave (walls battleCave) (Map.delete coordinate (elves battleCave)) (goblins battleCave))
  | otherwise = (Nothing, battleCave)

orderNpcs :: Elves -> Goblins -> [Coordinate]
orderNpcs elves goblins = List.sort ((Map.keys elves) ++ (Map.keys goblins))

moveNpc :: Npc -> BattleCave -> Npc
moveNpc npc battleCave = do
  if species npc == Elf
    then do
      let shortestPath = findShortestPathToOpponent npc (Set.union (walls battleCave) (Set.fromList (Map.keys (elves battleCave)))) (Set.fromList $ Map.keys $ goblins battleCave)
      if isNothing shortestPath then npc
      else Npc (takeSecond (pathSoFar (fromJust shortestPath))) (hitPoints npc) (species npc) (attackPower npc)
    else do
      let shortestPath = findShortestPathToOpponent npc (Set.union (walls battleCave) (Set.fromList (Map.keys (goblins battleCave)))) (Set.fromList $ Map.keys $ elves battleCave)
      if isNothing shortestPath then npc
      else Npc (takeSecond (pathSoFar (fromJust shortestPath))) (hitPoints npc) (species npc) (attackPower npc)

findShortestPathToOpponent :: Npc -> Set.Set Coordinate -> Set.Set Coordinate -> Maybe Path
findShortestPathToOpponent npc obstacles opponents = do
  let npcCoordinate = coordinate npc
  let foundPaths = findPaths (Seq.fromList [Path npcCoordinate (Seq.fromList [npcCoordinate])]) obstacles opponents (Set.empty) []
  if isEmpty foundPaths then Nothing
  else Just $ getBestPath foundPaths

getBestPath paths = minimum paths

-- todo: change return value path to maybe in case all opponents are dead? or check for empty elves and goblins beforehand
findPaths :: Seq.Seq Path -> Set.Set Coordinate -> Set.Set Coordinate -> Set.Set Coordinate -> [Path] -> [Path]
findPaths paths obstacles opponents coordinatesPathedTo foundPaths
  | Seq.length paths == 0 = foundPaths
  | isAdjacentToOpponent currentPath opponents = do
    if (isEmpty foundPaths) || ((Seq.length $ pathSoFar currentPath) <= (Seq.length $ pathSoFar $ head foundPaths))
    then findPaths restOfPaths obstacles opponents coordinatesPathedTo (currentPath:foundPaths)
    else foundPaths
  | otherwise = do
    let nextSteps = getNextStepsOfCurrentPath currentPath obstacles coordinatesPathedTo
    let pathsToAdd = Seq.fromList $ map (\c -> toPath c currentPath) nextSteps
    findPaths (restOfPaths Seq.>< pathsToAdd) obstacles opponents (Set.union (Set.fromList nextSteps) coordinatesPathedTo) foundPaths
  where (currentPath, restOfPaths) = splitFirstAndRest paths

getNextStepsOfCurrentPath currentPath obstacles coordinatesPathedTo = do
  let freeCoordinates = getSurroundingCoordinates (currentPosition currentPath)
  let coordinatesNotPathedToAlready = filter (\c -> c `Set.notMember` coordinatesPathedTo) freeCoordinates
  let unblockedCoordinates = filter (\c -> c `notElem` obstacles) coordinatesNotPathedToAlready
  unblockedCoordinates

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
parseBattleCave rawInput elfPower = toCaveLine (lines rawInput) 0 elfPower emptyBattleCave

toCaveLine :: [String] -> Int -> Int -> BattleCave -> BattleCave
toCaveLine [] _ _ battleCave = battleCave
toCaveLine (rawLine:rawLines) y elfPower battleCave = toCaveLine rawLines (y + 1) elfPower (parseChar rawLine 0 y elfPower battleCave)

parseChar :: String -> Int -> Int -> Int -> BattleCave -> BattleCave
parseChar [] _ _ _ battleCave = battleCave
parseChar (char:chars) x y elfPower battleCave
  | char == '.' = parseChar chars (x + 1) y elfPower battleCave
  | char == '#' = parseChar chars (x + 1) y elfPower (BattleCave (Set.insert currentCoordinate (walls battleCave)) (elves battleCave) (goblins battleCave))
  | char == 'E' = parseChar chars (x + 1) y elfPower (BattleCave (walls battleCave) (Map.insert currentCoordinate (Npc currentCoordinate 200 Elf elfPower) (elves battleCave)) (goblins battleCave))
  | char == 'G' = parseChar chars (x + 1) y elfPower (BattleCave (walls battleCave) (elves battleCave) (Map.insert currentCoordinate (Npc currentCoordinate 200 Goblin 3) (goblins battleCave)))
  | otherwise = error ("unknown char" ++ (show char))
  where currentCoordinate = Coordinate x y

data BattleCave =  BattleCave {
  walls :: Set.Set Coordinate,
  elves :: Elves,
  goblins :: Goblins
} deriving (Show, Eq)

data Npc = Npc {
  coordinate :: Coordinate,
  hitPoints :: Int,
  species :: Species,
  attackPower :: Int
} deriving (Eq)

instance Show Npc where
  show (Npc _ hitPoints species _) = "hp" ++ (show hitPoints) ++ " " ++ (show species)

instance Ord Npc where
  compare (Npc c1 hp1 _ _) (Npc c2 hp2 _ _)
    | hp1 == hp2 = compare c1 c2
    | otherwise = compare hp1 hp2

data Path = Path {
  currentPosition :: Coordinate,
  pathSoFar :: Seq.Seq Coordinate
} deriving (Eq, Show)

instance Ord Path where
  compare (Path pos1 path1) (Path pos2 path2)
    | (Seq.length path1) == (Seq.length path2) = compare pos1 pos2
    | otherwise = compare (Seq.length path1) (Seq.length path2)

data Species = Elf | Goblin deriving (Show, Eq)

type Elves = Map.Map Coordinate Npc
type Goblins = Map.Map Coordinate Npc

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

isEmpty :: [a] -> Bool
isEmpty list = length list == 0