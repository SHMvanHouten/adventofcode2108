module AOC.Day11.PowerGridSpec where

import Test.Hspec
import Test.QuickCheck

import AOC.Day11.PowerGrid
import AOC.Util.Coordinate
import qualified Data.Map as Map

main :: IO ()
main = hspec spec

-- puzzle input is 4842
-- XByX {topLCoordinate = Coordinate {x' = 237, y' = 281}, size = 10, theTotalPower = 96}
spec :: Spec
spec = do

  describe "find the top left corner of the highest power " $ do
    it "will be (90,269), size 16 given serial number 18" $ do
      findTopLeftOfHighestPowerSquareOfAnySize 18 `shouldBe` XByX (Coordinate 90 269) 16 113

--  describe "find the top left corner of the highest power " $ do
--    it "will be (232,251), size 12 given serial number 42" $ do
--      findTopLeftOfHighestPowerSquareOfAnySize 42 `shouldBe` XByX (Coordinate 232 251) 12 119

  describe "find the top left corner of the highest power " $ do
    it "will be (33,45) given serial number 18" $ do
      findTopLeftOfHighestPowerSquare 18 `shouldBe` Coordinate 33 45

  describe "find the top left corner of the highest power " $ do
      it "will be (232,251), size 12 given serial number 42" $ do
        let grid = buildGrid
        let powerCells = Map.fromList $ map (\x -> (coordinate x, x)) $ map (\c -> toPowerCell c 42) grid
        let powerSquares = findPowerSquaresForSquareSize powerCells 12
        maximum powerSquares `shouldBe` XByX (Coordinate 232 251) 12 119

  describe "Calculate power level" $ do
    it "Fuel cell at 3 5 with grid serial number 8 has power level 4" $ do
          getPowerLevel (Coordinate 3 5) 8 `shouldBe` 4
    it "Fuel cell at 122 79 with grid serial number 57 has power level -5" $ do
          getPowerLevel (Coordinate 122 79) 57 `shouldBe` (-5)
    it "Fuel cell at 217 196 with grid serial number 39 has power level 0" $ do
          getPowerLevel (Coordinate 217 196) 39 `shouldBe` 0
    it "Fuel cell at 101 153 with grid serial number 71 has power level 4" $ do
          getPowerLevel (Coordinate 101 153) 71 `shouldBe` 4

  describe "Calculate Three By Three Power" $ do
    it "is 29 for coordinate (33,45) on grid 18" $ do
      let powerCellsMap = Map.fromList $ map (\x -> (coordinate x, x)) $ map (\c -> toPowerCell c 18) buildGrid
      getXByXPower (Coordinate 33 45) 3 powerCellsMap `shouldBe` 29

  describe "Calculate square Power" $ do
      it "is 113 for coordinate (90,269) at size 16 on grid 18" $ do
        let powerCellsMap = Map.fromList $ map (\x -> (coordinate x, x)) $ map (\c -> toPowerCell c 18) buildGrid
        getXByXPower (Coordinate 90 269) 16 powerCellsMap `shouldBe` 113

  describe "Calculate square Power" $ do
      it "is 119 for coordinate (232,251) at size 12 on grid 42" $ do
        let powerCellsMap = Map.fromList $ map (\x -> (coordinate x, x)) $ map (\c -> toPowerCell c 42) buildGrid
        getXByXPower (Coordinate 232 251) 12 powerCellsMap `shouldBe` 119

--Fuel cell at 3,5      Grid serial number 8 : power level 4
--Fuel cell at 122,79, grid serial number 57: power level -5.
--Fuel cell at 217,196, grid serial number 39: power level  0.
--Fuel cell at 101,153, grid serial number 71: power level  4.

--Find the fuel cell's rack ID, which is its X coordinate plus 10.
--Begin with a power level of the rack ID times the Y coordinate.
--Increase the power level by the value of the grid serial number (your puzzle input).
--Set the power level to itself multiplied by the rack ID.
--Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
--Subtract 5 from the power level.

--For grid serial number 18, the largest total 3x3 square has a top-left corner of 33,45 (with a total power of 29)

--For grid serial number 18, the largest total square (with a total power of 113) is 16x16 and has a top-left corner of 90,269, so its identifier is 90,269,16.
--For grid serial number 42, the largest total square (with a total power of 119) is 12x12 and has a top-left corner of 232,251, so its identifier is 232,251,12.