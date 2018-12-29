module AOC.Day22.Spelunking where

import AOC.Util.Coordinate
import Data.Map (Map, fromList, insert, elems, (!), map, empty, member, lookup, keys)
import qualified Data.Sequence as Seq
import Data.Maybe
import AOC.Util.SequenceHelper
import Data.List (any)

unfoundTargetNode :: Coordinate -> Node
unfoundTargetNode coordinate = Node coordinate stubNode Torch (1500)

stubNode :: Node
stubNode = Node (Coordinate (-1) (-1)) stubNode Torch 0

findQuickestPath :: State -> ActiveNodes -> Node -> Node
findQuickestPath state (n Seq.:<| ns) targetNode = do
  let nextRegions = getTargetRegions n (regionMap state) (previous n)
  let (updatedNextNodes, updatedTargetNode, newState) = updateNodes n nextRegions ns targetNode state
  findQuickestPath newState updatedNextNodes updatedTargetNode

findQuickestPath _ empty targetNode = targetNode

updateNodes :: Node -> [(Coordinate, Region)] -> Seq.Seq Node -> Node -> State -> (Seq.Seq Node, Node, State)
updateNodes node newRegions activeNodes targetNode state = do
  let newNodes = concatMap (`toPossibleNodes` node) newRegions
  let (updatedNewNodes, updatedTargetNode) = updateTargetNode newNodes targetNode []
  let (filteredNewNodes, newState) = updateVisitedCoordinates updatedNewNodes state Seq.empty
--  let updatedActiveNodes = filterOutSlowActiveNodes activeNodes filteredNewNodes
  let newActiveNodeSequence = Seq.filter (\n -> (currentTime n) < (currentTime targetNode))
                              $ activeNodes Seq.>< filteredNewNodes
  (newActiveNodeSequence, updatedTargetNode, newState)

updateVisitedCoordinates [] state filteredNodes = (filteredNodes, state)
updateVisitedCoordinates (n:ns) state filteredNodes
  | not $ locNTool `member` coords = updateVisitedCoordinates ns (addNodeToVisited n state) (filteredNodes Seq.|>n)
  | n `isFasterThan` (coords!locNTool) = updateVisitedCoordinates ns (addNodeToVisited n state) (filteredNodes Seq.|>n)
  | otherwise = updateVisitedCoordinates ns state filteredNodes
  where coords = visitedCoordinates state
        locNTool = (location n, currentTool n)

isFasterThan node time = (currentTime node) < time

addNodeToVisited node state = do
  let visited = visitedCoordinates state
  let updatedVisited = insert (location node, currentTool node) (currentTime node) visited
  State (regionMap state) (target state) updatedVisited

filterOutSlowActiveNodes :: Seq.Seq Node -> Seq.Seq Node -> Seq.Seq Node
filterOutSlowActiveNodes activeNodes newNodes = do
  Seq.filter (\n -> not $ any (`equalsAndIsFasterThan` n) newNodes) activeNodes

equalsAndIsFasterThan filteredNode checkNode
  | checkNode /= filteredNode = False
  | otherwise = (currentTime checkNode) <= (currentTime filteredNode)

updateTargetNode :: [Node] -> Node -> [Node]-> ([Node], Node)
updateTargetNode [] targetNode updatedNewNodes = (updatedNewNodes, targetNode)
updateTargetNode (n:ns) targetNode updatedNewNodes
  | n == targetNode && n < targetNode = updateTargetNode ns n updatedNewNodes
  | n == targetNode && n >= targetNode = updateTargetNode ns targetNode updatedNewNodes
  | otherwise = updateTargetNode ns targetNode (updatedNewNodes ++ [n])

toPossibleNodes :: (Coordinate, Region) -> Node -> [Node]
toPossibleNodes (coordinate, region) previousNode = do
  let tool = currentTool previousNode
  let time = (currentTime previousNode + 1)
  let sameToolNode = Node coordinate previousNode tool time
  let switchedToolNode = Node coordinate previousNode (getOtherToolForRegion tool region) (time + 7)
  [sameToolNode, switchedToolNode]

getOtherToolForRegion tool region = head $ filter (\t-> t /= tool && t /= (regionToolNotAllowedMap!region))allTools

getPossibleTargets node = filter (\c -> c /= location (previous node))
                          $ getSurroundingCoordinates (location node)

getTargetRegions:: Node -> Map Coordinate Region -> Node -> [(Coordinate, Region)]
getTargetRegions node regionMap previousNode = filter (`stepIsAllowed` (currentTool node))
                                       $ filter (\cToR -> fst cToR /= (location previousNode))
                                       $ getSurroundingRegions (location node) regionMap

getSurroundingRegions :: Coordinate -> Map Coordinate Region -> [(Coordinate, Region)]
getSurroundingRegions coord regionMap = Prelude.map (\p -> (fst p, fromJust $ snd p))
                                        $ filter (\p -> isJust $ snd p)
                                        $ Prelude.map (\c -> (c, c `Data.Map.lookup` regionMap))
                                        $ getSurroundingCoordinates coord

stepIsAllowed (_, region) tool = regionToolNotAllowedMap!region /= tool

buildState depth target = do
  let start = Coordinate 0 0
  let erosionLevels = insert target 0 $ getErosionLevelMap [(x' start)..(x' target+ 40)] [(y' start)..(y' target + 20)] depth
  let regionMap = Data.Map.map (toRegion) erosionLevels
  State regionMap target empty

none x func coll = not $ any (func) coll
--------
-- part 1
--------

determineRisk :: Coordinate -> Coordinate -> Int -> Int
determineRisk start target depth = do
  let erosionLevels = insert target 0 $ getErosionLevelMap [(x' start)..(x' target)] [(y' start)..(y' target)] depth
  sum $ Prelude.map (`mod` 3) $ elems erosionLevels

toRiskLevel region
  | region == Rocky = 0
  | region == Wet = 1
  | region == Narrow = 2

toRegion erosionLevel
    | erosionLevel `mod` 3 == 0 = Rocky
    | erosionLevel `mod` 3 == 1 = Wet
    | erosionLevel `mod` 3 == 2 = Narrow

determineGeologicIndex coordinate erosionMap
  | coordinate == Coordinate 0 0 = 0
  | y' coordinate == 0 = (x' coordinate) * 16807
  | x' coordinate == 0 = (y' coordinate) * 48271
  | otherwise = (erosionMap !(moveLeft coordinate)) * (erosionMap !(moveUp coordinate))

determineErosionLevel geologicIndex depth = (geologicIndex + depth) `mod` 20183

getErosionLevelMap :: [Int] -> [Int] -> Int -> Map Coordinate Int
getErosionLevelMap xRange yRange depth = toErosionLevelMap xRange yRange depth empty

toErosionLevelMap :: [Int] -> [Int] -> Int -> Map Coordinate Int -> Map Coordinate Int
toErosionLevelMap _ [] _ erosionLevelMap = erosionLevelMap
toErosionLevelMap xRange (y:ys) depth geoLogicIndexMap = toErosionLevelMap xRange ys depth (toGeologicIndexMapLine xRange y depth geoLogicIndexMap)

toGeologicIndexMapLine :: [Int] -> Int -> Int -> Map Coordinate Int -> Map Coordinate Int
toGeologicIndexMapLine [] _ _ geologicIndexMap = geologicIndexMap
toGeologicIndexMapLine (x:xs) y depth geologicIndexMap = toGeologicIndexMapLine xs y depth (insert coordinate (determineErosionLevel geologicIndex depth) geologicIndexMap)
  where coordinate = Coordinate x y
        geologicIndex = determineGeologicIndex coordinate geologicIndexMap

data Region = Rocky | Wet | Narrow deriving (Eq, Ord, Show)

data State = State {
  regionMap :: Map Coordinate Region,
  target :: Coordinate,
  visitedCoordinates :: Map (Coordinate, Tool) (TotalTimeToGetToCoordinate)
} deriving (Show)

data Node = Node {
  location :: Coordinate,
  previous :: Node,
  currentTool :: Tool,
  currentTime :: TotalTimeToGetToCoordinate
}

instance Eq Node where
  (Node l1 _ t1 _) == (Node l2 _ t2 _) = l1 == l2 && t1 == t2

instance Ord Node where
  compare (Node _ _ _ time1) (Node _ _ _ time2) = compare time1 time2

instance Show Node where
  show (Node loc _ tool time) = ((show (x' loc)) ++ "," ++ (show (y' loc))) ++ ":" ++ (show tool) ++ show time ++ "; "

type TotalTimeToGetToCoordinate = Int

type ActiveNodes = Seq.Seq Node

data Tool = ClimbingGear | Torch | Neither deriving (Eq, Ord, Show)

regionToolNotAllowedMap = fromList [(Rocky, Neither),
                                    (Wet, Torch),
                                    (Narrow, ClimbingGear)]

allTools = [ClimbingGear, Torch, Neither]

--printQuickestPath :: State -> ActiveNodes -> Node -> IO()
--printQuickestPath state (n Seq.:<| ns) targetNode = do
--  let nextRegions = getTargetRegions n (regionMap state) (keys $ path n)
--  let (updatedNextNodes, updatedTargetNode) = updateNodes n nextRegions ns targetNode
--  print updatedNextNodes
--  printQuickestPath state updatedNextNodes updatedTargetNode
--
--printQuickestPath _ empty targetNode = print targetNode