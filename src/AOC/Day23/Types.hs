module AOC.Day23.Types where

import AOC.Util.Coord3D
import Data.Set

-- Axis aligned bounding box https://gdbooks.gitbooks.io/3dcollisions/content/Chapter1/aabb.html
data Box = Box {
  minCoord :: Coord3d,
  maxCoord :: Coord3d
} deriving (Show, Eq)

data BoxWithBots = BoxWithBots {
  box :: Box,
  bots :: Set NanoBot
} deriving (Show, Eq)

data NanoBot = NanoBot {
  coord :: Coord3d,
  radius :: Int
} deriving (Show, Eq)

instance Ord NanoBot where
  compare (NanoBot _ r1) (NanoBot _ r2) = compare r1 r2
