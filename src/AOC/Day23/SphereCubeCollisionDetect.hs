module AOC.Day23.SphereCubeCollisionDetect where

import AOC.Util.Coord3D
import AOC.Day23.Types
import Data.Set

findBotsThatCollideWithBox :: Box -> Set NanoBot -> Set NanoBot
findBotsThatCollideWithBox box bots = Data.Set.filter (`botCollidesWithBox` box) bots

botCollidesWithBox :: NanoBot -> Box -> Bool
botCollidesWithBox (NanoBot botCenter r) box = do
                   let closestPoint = findClosestPointOnBoxToBotCenter botCenter box
                   let distanceToBox = distance botCenter closestPoint
                   distanceToBox <= r

findClosestPointOnBoxToBotCenter
  (Coord3d x y z)
  (Box (Coord3d minx miny minz) (Coord3d maxx maxy maxz)) = do
    let closestX = closestOnAxis x minx maxx
    let closestY = closestOnAxis y miny maxy
    let closestZ = closestOnAxis z minz maxz
    Coord3d closestX closestY closestZ

closestOnAxis a mina maxa
  | a > maxa = maxa
  | a < mina = mina
  | otherwise = a
