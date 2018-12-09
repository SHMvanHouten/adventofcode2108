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
  describe "parseNodes" $ do
    it "it should parse the raw input to nodes" $ do
      parseNodes testInput `shouldBe` [(Node 'A' 2 [1,1,2]), (Node 'B' 0 [10,11,12]), (Node 'C' 1 [2]), (Node 'D' 0 [99])]


testInput = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

--A, which has 2 child nodes (B, C) and 3 metadata entries (1, 1, 2).
--B, which has 0 child nodes and 3 metadata entries (10, 11, 12).
--C, which has 1 child node (D) and 1 metadata entry (2).
--D, which has 0 child nodes and 1 metadata entry (99).