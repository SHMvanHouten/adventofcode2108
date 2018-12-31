module AOC.Day24.ImmuneSystemSimSpec where

import Test.Hspec
import Test.QuickCheck

import AOC.Day24.ImmuneSystemSim
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- not 13437
  describe "challenge" $ do
    it "solves the challenge part 1" $ do
      input <- readFile "resources/input-day24.txt"
      let (immuneSystemGroups, infectionGroups) = parseInput input
      let result = resolveBattle immuneSystemGroups infectionGroups
      print result
      result > 12516 `shouldBe` True
      result < 13440 `shouldBe` True
      result `shouldNotBe` 13437

  describe "testInput" $ do
        it "solves the test input" $ do
          let (immuneSystemGroups, infectionGroups) = parseInput testInput
          let result = resolveBattle immuneSystemGroups infectionGroups
          result `shouldBe` 5216

--  describe "printFight" $ do
--    it "prints the fight" $ do
--      input <- readFile "resources/input-day24.txt"
--      let (immuneSystemGroups, infectionGroups) = parseInput input
--      printFight (mapIdToIdentity immuneSystemGroups) (mapIdToIdentity infectionGroups)

  describe "performAttacks" $ do
      it "Performs attacks according to order picked" $ do
        let (immuneSystemGroups, infectionGroups) = parseInput testInput
        let immuneAttacks = Map.fromList [(0,1), (1,0)]
        let infectionAttacks = Map.fromList [(1,1), (0,0)]
        let (resultImmuneAttacks, resultInfectionAttacks) = performAttacks (mapIdToIdentity immuneSystemGroups) (mapIdToIdentity infectionGroups) (immuneAttacks, infectionAttacks)
        Map.size resultImmuneAttacks `shouldBe` 1
        (amountOfUnits $ resultImmuneAttacks Map.! 1) `shouldBe` 905
        (amountOfUnits $ resultInfectionAttacks Map.! 0) `shouldBe` 797
        (amountOfUnits $ resultInfectionAttacks Map.! 1) `shouldBe` 4434

  describe "findOrderOfAttacks" $ do
        it "finds the order the attacks will be performed in" $ do
          let (immuneSystemGroups, infectionGroups) = parseInput testInput
          let order = findOrderOfAttacks (immuneSystemGroups ++ infectionGroups)
          let expectedOrder = [(Infection, 1), (ImmuneSystem, 1), (ImmuneSystem, 0), (Infection, 0)]
          order `shouldBe` expectedOrder

  describe "findWhoAttacksWho" $ do
        it "each group picks their target" $ do
          let (immuneSystemGroups, infectionGroups) = parseInput testInput
          let (resultImmuneAttacks, resultInfectionAttacks) = findWhoAttacksWho immuneSystemGroups infectionGroups
          let expectedImmuneAttacks = Map.fromList [(0,1), (1,0)]
          let expectedInfectionAttacks = Map.fromList [(1,1), (0,0)]
          resultImmuneAttacks `shouldBe` expectedImmuneAttacks
          resultInfectionAttacks `shouldBe` expectedInfectionAttacks

  describe "target selection" $ do
      it "selects the target with the highest effective power if damage dealt would be equal" $ do
        let (immuneSystemGroups, infectionGroups) = parseInput testInput
        let pickingInfection = head infectionGroups
        let choice = chooseOpposingGroupToInfect pickingInfection immuneSystemGroups
        (id' $ fromJust choice) `shouldBe` 0
      it "selects the target that would receive the most damage" $ do
        let (immuneSystemGroups, infectionGroups) = parseInput testInput
        let pickingImmune = head immuneSystemGroups
        let choice = chooseOpposingGroupToInfect pickingImmune infectionGroups
        (id' $ fromJust choice) `shouldBe` 1
      it "selects no target if the damage dealt would be 0" $ do
        let attackingGroup = Group 0 Infection 10 10 [] [] "fire" 9 1
        let defGroup = Group 0 ImmuneSystem 1 1 [] ["fire"] "fire" 9 1
        let choice = chooseOpposingGroupToInfect attackingGroup [defGroup]
        choice `shouldBe` Nothing
  describe "calculateDamage" $ do
    it "calculates double damage if damageType is a weakness" $ do
      let attackingGroup = Group 0 Infection 10 10 [] [] "fire" 9 1
      let defGroup = Group 0 ImmuneSystem 1 1 ["fire"] [] "fire" 9 1
      calculateDamage defGroup attackingGroup `shouldBe` 180
    it "calculates normal damage if damageType is neither weakness nor immunity" $ do
      let attackingGroup = Group 0 Infection 10 10 [] [] "fire" 9 1
      let defGroup = Group 0 ImmuneSystem 1 1 [] [] "fire" 9 1
      calculateDamage defGroup attackingGroup `shouldBe` 90
    it "calculates no damage if damageType is immunity" $ do
      let attackingGroup = Group 0 Infection 10 10 [] [] "fire" 9 1
      let defGroup = Group 0 ImmuneSystem 1 1 [] ["fire"] "fire" 9 1
      calculateDamage defGroup attackingGroup `shouldBe` 0

--  describe "parse input" $ do
--    it "parses the test input" $ do
--      let (immuneSystemGroups, infectionGroups) = parseInput testInput
--      putStrLn $ unlines $ map (show) immuneSystemGroups
--      putStrLn $ unlines $ map (show) infectionGroups
--      pending

  describe "effectivePower" $ do
      it "the effective power of 17 units with ap 4507 is 76619" $ do
        let group = read "Group {id' = 0, groupType = ImmuneSystem, amountOfUnits = 17, hitPoints = 5390, weaknesses = [\"radiation\",\"bludgeoning\"], immunities = [], damageType = \"fire\", damagePower = 4507, initiative = 2}" :: Group
        effectivePower group `shouldBe` 76619

testInput = "Immune System:\n"++
            "17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2\n"++
            "989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3\n"++
            "Infection:\n"++
            "801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1\n"++
            "4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4\n"