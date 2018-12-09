module AOC.Day08.LicenseTree where

import Data.List
import Data.Char
import qualified Data.Map as Map

getValueOfRootNode rawInput = do
  let input = map (parseInt) (words rawInput)
  let rootNode = getRootNode input
  getValueOfNode rootNode

getValueOfNode :: Node -> Int
getValueOfNode node
  | length (childNodes node) == 0 = sum (metaData node)
  | otherwise = getValueOfRelevantChildNodes (childNodes node) (metaData node)

getValueOfRelevantChildNodes :: [Node] -> [Int] -> Int
getValueOfRelevantChildNodes childNodes metaData = do
  let metaDataThatReferenceChildNodes = filter (\x -> correspondsToChildNode x childNodes) metaData
  let metaDataRefToValues = Map.fromList (map (\x -> (x, getValueOfNode (childNodes!!(x - 1)))) (nub metaDataThatReferenceChildNodes))
  sum (map (\x -> metaDataRefToValues Map.!x) metaDataThatReferenceChildNodes)

correspondsToChildNode :: Int -> [Node] -> Bool
-- we start counting at 1 (#offByOne)
correspondsToChildNode metaDataNr nodes = (metaDataNr <= (length nodes)) && metaDataNr > 0
----------
-- part 2
----------
getSumOfAllMetaDataEntries rawInput = do
  let nodes = parseNodes rawInput
  sum (concatMap (metaData) nodes)

parseNodes :: String -> [Node]
parseNodes rawInput = do
  let input = map (parseInt) (words rawInput)
  let rootNode = getRootNode input
  getAllPosterity rootNode []

getAllPosterity :: Node -> [Node] -> [Node]
getAllPosterity currentNode nodesSoFar = do
  currentNode:nodesSoFar ++ (concatMap (\x -> getAllPosterity x []) (childNodes currentNode))

getRootNode :: [Int] -> Node
getRootNode input = fst (parseNodeTree input 'A' 'B')

parseNodeTree :: [Int] -> Char -> Char -> (Node, [Int])
parseNodeTree input id firstChildId = do
  let headerAndRestOfInput = splitAt 2 input
  let header = fst headerAndRestOfInput
  let childNodesAmount = header!!0
  let metaDataSize = header!!1

  let restOfInput = snd headerAndRestOfInput
  let childNodesToInputRemainder = getChildNodes restOfInput childNodesAmount [] firstChildId
  let metaDataToRemainingInput = splitAt metaDataSize (snd childNodesToInputRemainder)

  (Node id (fst childNodesToInputRemainder) (fst metaDataToRemainingInput), snd metaDataToRemainingInput)

getChildNodes :: [Int] -> Int -> [Node] -> Char -> ([Node], [Int])
getChildNodes input amount childNodesSoFar currentId
  | amount == 0 = ([], input)
  | amount == 1 = do
    let nextChar = getCharXRemoved currentId amount
    let currentNodeToRemainingInput = parseNodeTree input currentId nextChar
    (childNodesSoFar++[fst currentNodeToRemainingInput], snd currentNodeToRemainingInput)
  | otherwise = do
      let nextChar = getCharXRemoved currentId amount
      let currentNodeToRemainingInput = parseNodeTree input currentId nextChar
      getChildNodes (snd currentNodeToRemainingInput) (amount - 1) (childNodesSoFar ++ [fst currentNodeToRemainingInput]) (getNextChar currentId)

parseInt str = read str :: Int

getNextChar char = getCharXRemoved char 1
getCharXRemoved char int = chr (ord char + int)

data Node = Node {
  identity :: Char,
  childNodes :: [Node],
  metaData :: [Int]
} deriving (Show, Eq)