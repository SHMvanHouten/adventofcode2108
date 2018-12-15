module AOC.Day14.Recipes where

import qualified Data.Sequence as Seq
import Data.List

cookForTheMagicRecipe :: String -> Int
cookForTheMagicRecipe magicRecipe = do
  let initialState = RecipeState 0 1 (Seq.fromList [3,7,1,0]) ('x':map (\_ -> 'x') magicRecipe)
  cookUntilWeFind initialState magicRecipe

cookUntilWeFind :: RecipeState -> String -> Int
cookUntilWeFind state magicString
  | (Seq.length $ recipeBook newState) > 30000000 = error "nope"
  | foundTheMagicString (theTail newState) magicString = Seq.length (recipeBook newState) - (length magicString)
  | otherwise = cookUntilWeFind newState magicString
  where newState = cook state

foundTheMagicString :: String -> String -> Bool
foundTheMagicString theTail magicString = magicString `isInfixOf` theTail

cookUntilN :: RecipeState -> Int -> String
cookUntilN state n
  | length (recipeBook newState) > n + 10 = calculateScore state n
  | otherwise = cookUntilN newState n
  where newState = cook state

calculateScore :: RecipeState -> Int -> String
calculateScore state n = stringify $ Seq.drop n recipes
  where recipes = recipeBook state

stringify :: RecipeBook -> String
stringify recipeOuttake = concatMap (show) recipeOuttake

cook :: RecipeState -> RecipeState
cook state = do
  let firstElf = elf1 state
  let secondElf = elf2 state
  let oldRecipes = recipeBook state

  let elf1Recipe = oldRecipes `Seq.index` firstElf
  let elf2Recipe = oldRecipes `Seq.index` secondElf

  let dishes = getDigits (elf1Recipe + elf2Recipe)

  let newRecipeBook = oldRecipes <> Seq.fromList dishes
  let recipesSize = length newRecipeBook

  let updatedTail = updateTail dishes (theTail state)
  RecipeState (move firstElf elf1Recipe recipesSize) (move secondElf elf2Recipe recipesSize) newRecipeBook updatedTail

updateTail :: [Int] -> String -> String
updateTail freshDishes oldTail =  drop (length freshDishes) $ oldTail ++ (concatMap (show) freshDishes)

getDigits :: Int -> [Int]
getDigits number
  | x == 0 = [y]
  | otherwise = [x,y]
  where (x,y) = divMod number 10

move :: Int -> Recipe -> Int -> Int
move elf recipeValue recipesSize = newIndex `mod` recipesSize
  where newIndex = (elf + recipeValue + 1)

data RecipeState = RecipeState {
  elf1 :: Int,
  elf2 :: Int,
  recipeBook :: RecipeBook,
  theTail :: String
} deriving (Show, Eq)

type Recipe = Int
type RecipeBook = Seq.Seq Recipe

