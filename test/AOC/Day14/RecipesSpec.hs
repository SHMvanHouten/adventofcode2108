module AOC.Day14.RecipesSpec where

import Test.Hspec
import Test.QuickCheck

import AOC.Day14.Recipes

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "it should solve the challenge input" $ do
    it "should solve the challenge" $ do
      pending

  describe "cook until we have n recipes before the last 10" $ do
--    it "after 5 recipes we get 5158916779" $ do
--      cookUntilN initialInput 5 `shouldBe` "0124515891"
    it "after 9 recipes we get 5158916779" $ do
      cookUntilN initialInput 9 `shouldBe` "5158916779"
    it "after 18 recipes we get 5158916779" $ do
        cookUntilN initialInput 18 `shouldBe` "9251071085"
    it "after 2018 recipes we get 5158916779" $ do
      cookUntilN initialInput 2018 `shouldBe` "5941429882"
--    it "solves the challenge input" $ do
--      cookUntilN initialInput 209231 `shouldBe` "6126491027"

  describe "cook until the magic string appears" $ do
    it "after 5 recipes we get " $ do
      cookUntilWeFind initialInput (reverse "01245") `shouldBe` 5
    it "after 9 recipes we get 01245" $ do
      cookUntilWeFind initialInput (reverse "51589") `shouldBe` 9
    it "after 18 recipes we get 92510" $ do
      cookUntilWeFind initialInput (reverse "92510") `shouldBe` 18
    it "after 2018 recipes we get 59414" $ do
      cookUntilWeFind initialInput (reverse "59414") `shouldBe` 2018
    it "solves the challenge input" $ do
      cookUntilWeFind initialInput (reverse "209231") `shouldBe` 2018

  describe "cook!" $ do
    it "cooks the recipe by combining the scores of the recipes the elves are standing on" $ do
      let input = RecipeState 0 1 (reverse [3,7])
      cook input `shouldBe` RecipeState 0 1 (reverse [3,7,1,0])
    it "cooks again" $ do
      let input = RecipeState 0 1 (reverse [3,7,1,0])
      cook input `shouldBe` RecipeState 4 3 (reverse [3,7,1,0,1,0])
    it "cooks again" $ do
      let input = RecipeState 4 3 (reverse [3,7,1,0,1,0])
      cook input `shouldBe` RecipeState 6 4 (reverse [3,7,1,0,1,0,1])
    it "cooks again" $ do
      let input = RecipeState 6 4 (reverse [3,7,1,0,1,0,1])
      cook input `shouldBe` RecipeState 0 6 (reverse [3,7,1,0,1,0,1,2])

initialInput = RecipeState 0 1 (reverse [3,7])
testInput = "(3)[7]\n"++
            "(3)[7] 1  0\n"++
            " 3  7  1 [0](1) 0\n"++
            " 3  7  1  0 [1] 0 (1)\n"++
            "(3) 7  1  0  1  0 [1] 2\n"++
            " 3  7  1  0 (1) 0  1  2 [4]\n"++
            " 3  7  1 [0] 1  0 (1) 2  4  5\n"++
            " 3  7  1  0 [1] 0  1  2 (4) 5  1\n"++
            " 3 (7) 1  0  1  0 [1] 2  4  5  1  5\n"++
            " 3  7  1  0  1  0  1  2 [4](5) 1  5  8\n"++
            " 3 (7) 1  0  1  0  1  2  4  5  1  5  8 [9]\n"++
            " 3  7  1  0  1  0  1 [2] 4 (5) 1  5  8  9  1  6\n"++
            " 3  7  1  0  1  0  1  2  4  5 [1] 5  8  9  1 (6) 7\n"++
            " 3  7  1  0 (1) 0  1  2  4  5  1  5 [8] 9  1  6  7  7\n"++
            " 3  7 [1] 0  1  0 (1) 2  4  5  1  5  8  9  1  6  7  7  9\n"++
            " 3  7  1  0 [1] 0  1  2 (4) 5  1  5  8  9  1  6  7  7  9  2\n"

--51589 first appears after 9 recipes.
--01245 first appears after 5 recipes.
--92510 first appears after 18 recipes.
--59414 first appears after 2018 recipes.