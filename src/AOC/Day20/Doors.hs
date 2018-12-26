module AOC.Day20.Doors where

import Data.Sequence
import AOC.Util.SequenceHelper
import AOC.Util.Coordinate

parseInput :: String -> [Path]
parseInput input = do
  let actualInput = init $ tail input
  parseRoutes actualInput (singleton (Coordinate 0 0)) []

parseRoutes :: String -> Path -> [Path] -> [Path]
parseRoutes [] pathSoFar routes = routes ++ [pathSoFar]
parseRoutes input pathSoFar routes
  | head input == '(' = do
    let (bracketStuff, rest) = findClosingBracket (tail input) [] 1
    let branches = findBranches bracketStuff 0 [] []
    routes ++ concatMap (\s -> parseRoutes (s++rest) pathSoFar []) branches
  | otherwise = do
    let (rawRoute, rest) = span (\c -> c /= '(') input
    parseRoutes rest (pathSoFar><(parseRoute rawRoute (lastElem pathSoFar))) routes

findClosingBracket (x:xs) foundSoFar openBrackets
  | x == ')' && openBrackets == 1 = (Prelude.reverse foundSoFar, xs)
  | x == ')' && openBrackets > 1 = findClosingBracket xs (x:foundSoFar) (openBrackets - 1)
  | x == '(' = findClosingBracket xs (x:foundSoFar) (openBrackets + 1)
  | otherwise = findClosingBracket xs (x:foundSoFar) openBrackets

findBranches :: String -> Int -> [Char] -> [[Char]] -> [[Char]]
findBranches [] _ currentBranch branches = branches ++ [(Prelude.reverse currentBranch)]
findBranches (x:xs) openBrackets currentBranch branches
  | x == '|' && openBrackets == 0 = findBranches xs openBrackets [] (branches ++ [(Prelude.reverse currentBranch)])
  | x == '(' = findBranches xs (openBrackets + 1) (x:currentBranch) branches
  | x == ')' = findBranches xs (openBrackets - 1) (x:currentBranch) branches
  | otherwise = findBranches xs openBrackets (x:currentBranch) branches

parseRoute:: String -> Coordinate -> Path
parseRoute input start = do
  let path = parsePath start input empty
  path

parsePath :: Coordinate -> [Char] -> Path -> Path
parsePath _ [] steps = steps
parsePath lastStep (x:xs) steps = parsePath nextStep xs (steps |> nextStep)
  where nextStep = goDirection x lastStep

goDirection char currentCoord
  | char == 'N' = moveUp currentCoord
  | char == 'E' = moveRight currentCoord
  | char == 'S' = moveDown currentCoord
  | char == 'W' = moveLeft currentCoord

type Path = Seq Coordinate

toPrintablePath path = findDirectionsTaken path []

findDirectionsTaken:: Path -> [Char] -> [Char]
findDirectionsTaken (x:<|y:<|xs) directions = findDirectionsTaken (y:<|xs) ((getDirection x y):directions)
findDirectionsTaken empty directions = Prelude.reverse directions

getDirection x y
  | moveUp x == y = 'N'
  | moveDown x == y = 'S'
  | moveLeft x == y = 'W'
  | moveRight x == y = 'E'