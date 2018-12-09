module AOC.Day08.LicenseTree where

parseNodes :: String -> [Node]
parseNodes rawInput = []

data Node = Node {
  identity :: Char,
  childNodes :: Int,
  metaData :: [Int]
} deriving (Show, Eq)