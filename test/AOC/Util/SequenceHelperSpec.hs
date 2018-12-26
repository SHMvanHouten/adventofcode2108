module AOC.Util.SequenceHelperSpec where

import Test.Hspec
import Test.QuickCheck

import AOC.Util.SequenceHelper
import Data.Sequence

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "lastElem" $ do
    it "takes the last element from the sequence" $ do
      let sequence = fromList [1,2,3,4,5,6]
      lastElem sequence `shouldBe` 6
    it "takes the only elem" $ do
      let sequence = fromList [6]
      lastElem sequence `shouldBe` 6
    it "fails on an empty sequence" $ do
      let sequence = empty
      lastElem sequence `shouldThrow` anyException


