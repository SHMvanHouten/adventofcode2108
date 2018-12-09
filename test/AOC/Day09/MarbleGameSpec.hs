module AOC.Day09.MarbleGameSpec where

import Test.Hspec
import Test.QuickCheck

import AOC.Day09.MarbleGame

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "getWinningElfScore" $ do
    it "the winning elf should score 32" $ do
      getWinningElfScore 9 25 `shouldBe` 32
    it "the winning elf should score 32" $ do
      getWinningElfScore 10 1618 `shouldBe` 8317
    it "the winning elf should score 32" $ do
      getWinningElfScore 13 7999 `shouldBe` 146373
    it "the winning elf should score 32" $ do
      getWinningElfScore 17 1104 `shouldBe` 2764
    it "the winning elf should score 32" $ do
      getWinningElfScore 21 6111 `shouldBe` 54718
    it "the winning elf should score 32" $ do
      getWinningElfScore 30 5807 `shouldBe` 37305


--9 players; last marble is worth 25 points: high score is 32
--10 players; last marble is worth 1618 points: high score is 8317
--13 players; last marble is worth 7999 points: high score is 146373
--17 players; last marble is worth 1104 points: high score is 2764
--21 players; last marble is worth 6111 points: high score is 54718
--30 players; last marble is worth 5807 points: high score is 37305