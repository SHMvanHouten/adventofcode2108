module AOC.Day23.SphereCubeCollisionDetectSpec where

import Test.Hspec
import Test.QuickCheck
import AOC.Util.Coord3D
import AOC.Day23.SphereCubeCollisionDetect
import AOC.Day23.Types
import Data.Set

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
-- 113267018 too high Coord3d {x = 39044798, y = 25111110, z = 49111110}

  describe "finds the bots that collide with the box" $ do
    it "finds that bot one and 3 collide with the box" $ do
      let bot1 = NanoBot (Coord3d 3 1 1) 2
      let bot2= NanoBot (Coord3d 4 1 1) 2
      let bot3= NanoBot (Coord3d 2 2 2) 3
      let bots = [bot1, bot2, bot3]
      let box = Box (Coord3d 0 0 0) (Coord3d 1 1 1)
      findBotsThatCollideWithBox box (fromList bots) `shouldBe` fromList [bot1, bot3]