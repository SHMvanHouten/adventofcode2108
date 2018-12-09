module AOC.Day08.LicenseTreePart2Spec where

import Test.Hspec
import Test.QuickCheck

import AOC.Day08.LicenseTree

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "getValueOfRootNode" $ do
    it "should find the value of 66 for the root node" $ do
      getValueOfRootNode testInput `shouldBe` 66

  describe "getValueOfNode" $ do
    it "should get the sum of the metadata if node has no children" $ do
      let node = Node 'A' [] [5,5]
      getValueOfNode node `shouldBe` 10
    it "should get the value of the child if the node has a child which and it references it" $ do
       let child = Node 'B' [] [5,5]
       let node = Node 'A' [child] [1,1]
       getValueOfNode node `shouldBe` 20
    it "should get the value of 0 if it has a child but doesn't reference it" $ do
       let child = Node 'B' [] [5,5]
       let node = Node 'A' [child] [2,2]
       getValueOfNode node `shouldBe` 0
    it "should get the value of 66" $ do
       let nodeB = Node 'B' [] [10,11,12]
       let nodeD = Node 'D' [] [99]
       let nodeC = Node 'C' [nodeD] [2]
       let nodeA = Node 'A' [nodeB, nodeC] [1,1,2]
       getValueOfNode nodeA `shouldBe` 66



testInput = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

--A, which has 2 child nodes (B, C) and 3 metadata entries (1, 1, 2).
--B, which has 0 child nodes and 3 metadata entries (10, 11, 12).
--C, which has 1 child node (D) and 1 metadata entry (2).
--D, which has 0 child nodes and 1 metadata entry (99).