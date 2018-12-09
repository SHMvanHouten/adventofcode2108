module AOC.Day08.LicenseTree where

import Data.List
import Data.Char

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