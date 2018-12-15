module AOC.Day14.Recipes where

cookUntilWeFind :: RecipeState -> String -> Int
cookUntilWeFind state magicString
  | foundTheMagicString (recipes newState) magicString = length (recipes newState) - 5
  | otherwise = cookUntilWeFind newState magicString
  where newState = cook state

foundTheMagicString recipeBook magicString = do
  let magicLength = length magicString
  let string1 = concatMap (show) $ take magicLength recipeBook
  let string2 = concatMap (show) $ tail $ take (magicLength + 1) recipeBook

  magicString == string1 || magicString == string2

cookUntilN :: RecipeState -> Int -> String
cookUntilN state n
  | length (recipes newState) > n + 10 = calculateScore state n
  | otherwise = cookUntilN newState n
  where newState = cook state

calculateScore :: RecipeState -> Int -> String
calculateScore state n
  | length recipeBook > n + 10 = stringify $ reverse $ tail (take 11 recipeBook)
  | otherwise = stringify $ reverse $ take 10 recipeBook
  where recipeBook = recipes state

stringify :: [Recipe] -> String
stringify recipeOuttake = concatMap (show) recipeOuttake

cook :: RecipeState -> RecipeState
cook state = do
  let firstElf = elf1 state
  let secondElf = elf2 state
  let oldRecipes = recipes state

  let elf1Recipe = get (firstElf) (oldRecipes)
  let elf2Recipe = get (secondElf) (oldRecipes)

  let dishes = elf1Recipe + elf2Recipe
  let firstDish = dishes `div` 10
  let secondDish = dishes `mod` 10

  let newRecipeBook = addDishes firstDish secondDish oldRecipes
  let recipesSize = length newRecipeBook
  RecipeState (move firstElf elf1Recipe recipesSize) (move secondElf elf2Recipe recipesSize) newRecipeBook
move :: Int -> Recipe -> Int -> Int
move elf recipeValue recipesSize
  | newIndex < recipesSize = newIndex
  | otherwise = newIndex `mod` recipesSize
  where newIndex = (elf + recipeValue + 1)

addDishes :: Recipe -> Recipe -> [Recipe] -> [Recipe]
addDishes firstDish secondDish dishes
  | firstDish > 0 = add secondDish $ add firstDish dishes
  | otherwise = add secondDish dishes

data RecipeState = RecipeState {
  elf1 :: Int,
  elf2 :: Int,
  recipes :: [Recipe]
} deriving (Show, Eq)

type Recipe = Int

-- recipes will have to be added at the start so we have to reverse the list
add :: a -> [a] -> [a]
add item list = item:list

get :: Int -> [a] -> a
get i list = list!!(length list - (1+i))
