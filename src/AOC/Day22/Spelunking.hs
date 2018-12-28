module AOC.Day22.Spelunking where

import AOC.Util.Coordinate
import Data.Map (Map, fromList, insert, elems, (!), map, empty, member, lookup, keys)
import qualified Data.Sequence as Seq
import Data.Maybe
import AOC.Util.SequenceHelper
import Data.List (any)

unfoundTargetNode coordinate = Node coordinate empty Torch (10000000)

printQuickestPath :: State -> ActiveNodes -> Node -> IO()
printQuickestPath state (n Seq.:<| ns) targetNode = do
  let nextRegions = getTargetRegions n (regionMap state) (keys $ path n)
  let (updatedNextNodes, updatedTargetNode) = updateNodes n nextRegions ns targetNode
  print updatedNextNodes
  printQuickestPath state updatedNextNodes updatedTargetNode

printQuickestPath _ empty targetNode = print targetNode

findQuickestPath :: State -> ActiveNodes -> Node -> Node
findQuickestPath state (n Seq.:<| ns) targetNode = do
  let nextRegions = getTargetRegions n (regionMap state) (keys $ path n)
  let (updatedNextNodes, updatedTargetNode) = updateNodes n nextRegions ns targetNode
  findQuickestPath state updatedNextNodes updatedTargetNode

findQuickestPath _ empty targetNode = targetNode

updateNodes :: Node -> [(Coordinate, Region)] -> Seq.Seq Node -> Node -> (Seq.Seq Node, Node)
updateNodes node newRegions activeNodes targetNode = do
  let newNodes = concatMap (`toPossibleNodes` node) newRegions
  let (updatedNewNodes, updatedTargetNode) = updateTargetNode newNodes targetNode []
  let doubleUpdatedNewNodes = updatedNewNodes
  -- todo: filter out new nodes that are slower at their current location with their tool than active nodes had been at any point in their path
  let updatedActiveNodes = filterOutSlowActiveNodes activeNodes updatedNewNodes
  let newActiveNodeSequence = Seq.filter (\n -> (currentTime n) < (currentTime targetNode))
                              $ updatedActiveNodes Seq.>< (Seq.fromList doubleUpdatedNewNodes)
  (newActiveNodeSequence, updatedTargetNode)

filterOutSlowActiveNodes :: Seq.Seq Node -> [Node] -> Seq.Seq Node
filterOutSlowActiveNodes activeNodes newNodes = do
  let filteredOutCurrentLocation = Seq.filter (\n -> not $ any (`equalsAndIsFasterThan` n) newNodes) activeNodes
  Seq.filter (\n -> not $ any (`isFasterThanInPath` (path n)) newNodes) filteredOutCurrentLocation

equalsAndIsFasterThan checkNode filteredNode
  | checkNode /= filteredNode = False
  | otherwise = (currentTime checkNode) <= (currentTime filteredNode)

updateTargetNode :: [Node] -> Node -> [Node]-> ([Node], Node)
updateTargetNode [] targetNode updatedNewNodes = (updatedNewNodes, targetNode)
updateTargetNode (n:ns) targetNode updatedNewNodes
  | n == targetNode && n < targetNode = updateTargetNode ns n updatedNewNodes
  | n == targetNode && n >= targetNode = updateTargetNode ns targetNode updatedNewNodes
  | n `isFasterThanInPath` (path targetNode) = updateTargetNode ns (retrackNodesPathWithQuickerStep n targetNode) updatedNewNodes
  | otherwise = updateTargetNode ns targetNode (updatedNewNodes ++ [n])

retrackNodesPathWithQuickerStep :: Node -> Node -> Node
retrackNodesPathWithQuickerStep quickerNode oldNode
  | nextStep == Nothing = quickerNode
  | otherwise = retrackNodesPathWithQuickerStep (fromJust nextStep) oldNode
  where nextStep = findNextStep quickerNode oldNode

findNextStep newNode oldNode
  | any (`member` (path oldNode)) possibleTargets = do
    let nextStepLocation = head $ filter (`member` (path oldNode)) possibleTargets
    let nextStepTool = fst $ (path oldNode)!nextStepLocation
    let nextStepTimeChange = 1 + (getToolSwitchingTime (currentTool newNode) nextStepTool)
    let nextStepTime = currentTime newNode + nextStepTimeChange
    let newPath = insert nextStepLocation (nextStepTool, nextStepTime) (path newNode)
    Just $ Node nextStepLocation newPath nextStepTool nextStepTime
  | otherwise = Nothing
  where possibleTargets = getPossibleTargets newNode

getToolSwitchingTime oldTool newTool
  | oldTool == newTool = 0
  | otherwise = 7

isFasterThanInPath :: Node -> Map Coordinate (Tool, TotalTimeToGetToCoordinate) -> Bool
isFasterThanInPath node path
 | not $ loc `member` path = False
 | otherwise = do
    let toolToTime = path!loc
    (fst toolToTime == (currentTool node) ) && (snd toolToTime ) > (currentTime node)
 where loc = location node

toPossibleNodes :: (Coordinate, Region) -> Node -> [Node]
toPossibleNodes (coordinate, region) previousNode = do
  let tool = currentTool previousNode
  let updatedPath = insert (location previousNode) (tool, currentTime previousNode) (path previousNode)
  let time = (currentTime previousNode + 1)
  let sameToolNode = Node coordinate updatedPath tool time
  let switchedToolNode = Node coordinate updatedPath (getOtherToolForRegion tool region) (time + 7)
  [sameToolNode, switchedToolNode]

getOtherToolForRegion tool region = head $ filter (\t-> t /= tool && t /= (regionToolNotAllowedMap!region))allTools

getPossibleTargets node = filter (\c -> not $ any (==c) (keys $ path node))
                          $ getSurroundingCoordinates (location node)

getTargetRegions:: Node -> Map Coordinate Region -> [Coordinate] -> [(Coordinate, Region)]
getTargetRegions node regionMap visitedCoordinates = filter (`stepIsAllowed` (currentTool node))
                                       $ filter (\cToR -> not $ any (== (fst cToR)) visitedCoordinates)
                                       $ getSurroundingRegions (location node) regionMap

getSurroundingRegions :: Coordinate -> Map Coordinate Region -> [(Coordinate, Region)]
getSurroundingRegions coord regionMap = Prelude.map (\p -> (fst p, fromJust $ snd p))
                                        $ filter (\p -> isJust $ snd p)
                                        $ Prelude.map (\c -> (c, c `Data.Map.lookup` regionMap))
                                        $ getSurroundingCoordinates coord

stepIsAllowed (_, region) tool = regionToolNotAllowedMap!region /= tool

buildState depth target = do
  let start = Coordinate 0 0
  let erosionLevels = insert target 0 $ getErosionLevelMap [(x' start)..(x' target+ 20)] [(y' start)..(y' target + 20)] depth
  let regionMap = Data.Map.map (toRegion) erosionLevels
  State regionMap target

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
  target :: Coordinate
} deriving (Show)

data Node = Node {
  location :: Coordinate,
  path :: Map Coordinate (Tool, TotalTimeToGetToCoordinate),
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