module AOC.Day08.LicenseTreeSpec where

import Test.Hspec
import Test.QuickCheck

import AOC.Day08.LicenseTree

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "getSumOfAllMetaDataEntries" $ do
    it "should sum all the metadata of all the found nodes" $ do
      getSumOfAllMetaDataEntries testInput `shouldBe`138

  describe "parseNodes" $ do
    it "should parse the raw input to nodes" $ do
      let nodeB = Node 'B' [] [10,11,12]
      let nodeD = Node 'D' [] [99]
      let nodeC = Node 'C' [nodeD] [2]
      let nodeA = Node 'A' [nodeB, nodeC] [1,1,2]
      parseNodes testInput `shouldBe` [nodeA, nodeB, nodeC, nodeD]


testInput = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

--A, which has 2 child nodes (B, C) and 3 metadata entries (1, 1, 2).
--B, which has 0 child nodes and 3 metadata entries (10, 11, 12).
--C, which has 1 child node (D) and 1 metadata entry (2).
--D, which has 0 child nodes and 1 metadata entry (99).