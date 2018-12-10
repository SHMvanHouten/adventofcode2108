module AOC.Day10.StarMessageSpec where

import Test.Hspec
import Test.QuickCheck

import AOC.Day10.StarMessage

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "print stars" $ do
    it "the messate should be HI" $ do
      0 `shouldBe` 0

  describe "parseStar" $ do
    it "should parse the raw input to a list of stars" $ do
      let testLine = (lines testInput)!!0
      parseStar testLine `shouldBe` Star (Coordinate 9 1) (Velocity 0 2)


  describe "parseStars" $ do
    it "should parse the raw input to a list of stars" $ do
      let testLines = "position=< 9,  1> velocity=< 0,  2>\n\n"++
                 "position=< 7,  0> velocity=<-1,  0>"
      parseStars testLines `shouldBe` [Star (Coordinate 9 1) (Velocity 0 2), Star (Coordinate 7 0) (Velocity (-1) 0)]

  describe "parseCoordinate" $ do
    it "should parse the coordinate from raw input" $ do
      parseCoordinate " 9, 1> velocity=" `shouldBe` (Coordinate 9 1)
      parseCoordinate "-9,-1> velocity=" `shouldBe` (Coordinate (-9) (-1))
      parseCoordinate "100,100> velocity=" `shouldBe` (Coordinate 100 100)

  describe "removeSpace" $ do
    it "should remove the starting space from a str" $ do
      removeSpace " 9" `shouldBe` "9"

testInput = "position=< 9,  1> velocity=< 0,  2>\n"++
            "position=< 7,  0> velocity=<-1,  0>\n"++
            "position=< 3, -2> velocity=<-1,  1>\n"++
            "position=< 6, 10> velocity=<-2, -1>\n"++
            "position=< 2, -4> velocity=< 2,  2>\n"++
            "position=<-6, 10> velocity=< 2, -2>\n"++
            "position=< 1,  8> velocity=< 1, -1>\n"++
            "position=< 1,  7> velocity=< 1,  0>\n"++
            "position=<-3, 11> velocity=< 1, -2>\n"++
            "position=< 7,  6> velocity=<-1, -1>\n"++
            "position=<-2,  3> velocity=< 1,  0>\n"++
            "position=<-4,  3> velocity=< 2,  0>\n"++
            "position=<10, -3> velocity=<-1,  1>\n"++
            "position=< 5, 11> velocity=< 1, -2>\n"++
            "position=< 4,  7> velocity=< 0, -1>\n"++
            "position=< 8, -2> velocity=< 0,  1>\n"++
            "position=<15,  0> velocity=<-2,  0>\n"++
            "position=< 1,  6> velocity=< 1,  0>\n"++
            "position=< 8,  9> velocity=< 0, -1>\n"++
            "position=< 3,  3> velocity=<-1,  1>\n"++
            "position=< 0,  5> velocity=< 0, -1>\n"++
            "position=<-2,  2> velocity=< 2,  0>\n"++
            "position=< 5, -2> velocity=< 1,  2>\n"++
            "position=< 1,  4> velocity=< 2,  1>\n"++
            "position=<-2,  7> velocity=< 2, -2>\n"++
            "position=< 3,  6> velocity=<-1, -1>\n"++
            "position=< 5,  0> velocity=< 1,  0>\n"++
            "position=<-6,  0> velocity=< 2,  0>\n"++
            "position=< 5,  9> velocity=< 1, -2>\n"++
            "position=<14,  7> velocity=<-2,  0>\n"++
            "position=<-3,  6> velocity=< 2, -1>"