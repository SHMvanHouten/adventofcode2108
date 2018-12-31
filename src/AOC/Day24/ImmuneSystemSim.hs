module AOC.Day24.ImmuneSystemSim where

import Data.List.Split
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe

printFight :: IdsToGroups -> IdsToGroups -> IO()
printFight immunes infections
  | isEmpty resultImmunes = putStrLn $ unlines $ map (show) $ Map.elems resultInfections
  | isEmpty resultInfections = putStrLn $ unlines $ map (show) $ Map.elems resultImmunes
  | otherwise = do
    putStrLn $ unlines $ map (show) $ Map.elems resultImmunes
    putStrLn $ unlines $ map (show) $ Map.elems resultInfections
    printFight resultImmunes resultInfections
  where targets = findWhoAttacksWho (Map.elems immunes) (Map.elems infections)
        (resultImmunes, resultInfections) = performAttacks immunes infections targets

resolveBattle :: [Group] -> [Group] -> Int
resolveBattle immuneGroups infGroups = do
  let immuneGroupMap = mapIdToIdentity immuneGroups
  let infectionGroupMap = mapIdToIdentity infGroups
  let winners = fightUntilOneGroupTypeIsLeft immuneGroupMap infectionGroupMap
  sum $ map (amountOfUnits) (Map.elems winners)

fightUntilOneGroupTypeIsLeft :: IdsToGroups -> IdsToGroups -> IdsToGroups
fightUntilOneGroupTypeIsLeft immunes infections
  | isEmpty resultImmunes = resultInfections
  | isEmpty resultInfections = resultImmunes
  | otherwise = fightUntilOneGroupTypeIsLeft resultImmunes resultInfections
  where targets = findWhoAttacksWho (Map.elems immunes) (Map.elems infections)
        (resultImmunes, resultInfections) = performAttacks immunes infections targets

performAttacks :: IdsToGroups -> IdsToGroups -> (TargetsMap, TargetsMap) -> (IdsToGroups, IdsToGroups)
performAttacks immuneGroups infectGroups (immuneTargets, infectionTargets) = do
  let order = findOrderOfAttacks ((Map.elems immuneGroups) ++ (Map.elems infectGroups))
  let resultingGroupsAndTargets = attack order (GroupsAndTargets immuneGroups infectGroups immuneTargets infectionTargets)
  (immuneSystemGroups resultingGroupsAndTargets, infectionGroups resultingGroupsAndTargets)

attack :: [(GroupType, Int)] -> GroupsAndTargets -> GroupsAndTargets
attack [] groupsAndTargets = groupsAndTargets
attack (o:os) groupsAndTargets
  | isNothing currentAttacker || isNothing target = attack os groupsAndTargets
  | otherwise = do
    let attacker = fromJust currentAttacker
    let defender = fromJust target
    let updatedDefender = dealDamage attacker defender
    if (amountOfUnits updatedDefender) <= 0
      then attack os (removeGroup defender groupsAndTargets)
      else attack os (updateGroup updatedDefender groupsAndTargets)
  where (currentAttacker, target) = getGroupAndTarget o groupsAndTargets

dealDamage :: Group -> Group -> Group
dealDamage attacker defender = do
  let damage = calculateDamage defender attacker
  let units = amountOfUnits defender
  let hp = hitPoints defender
  let remainingUnits = (((units * hp) - damage) `divRoundUp` hp)
  setAmountOfUnits remainingUnits defender

divRoundUp :: Int -> Int -> Int
divRoundUp numerator denominator = do
  let (result, remainder) = numerator `divMod` denominator
  result + remainderBiggerThanZero remainder

remainderBiggerThanZero remainder
  | remainder > 0 = 1
  | otherwise = 0

removeGroup :: Group -> GroupsAndTargets -> GroupsAndTargets
removeGroup group groupsAndTargets
  | groupType group == ImmuneSystem = do
    let groups = immuneSystemGroups groupsAndTargets
    let updatedGroups = Map.delete (id' group) groups
    setImmuneSystemGroups updatedGroups groupsAndTargets
  | groupType group == Infection = do
    let groups = infectionGroups groupsAndTargets
    let updatedGroups = Map.delete (id' group) groups
    setInfectionGroups updatedGroups groupsAndTargets

updateGroup :: Group -> GroupsAndTargets -> GroupsAndTargets
updateGroup group groupsAndTargets
  | groupType group == ImmuneSystem = do
    let groups = immuneSystemGroups groupsAndTargets
    let updatedGroups = Map.insert (id' group) group groups
    setImmuneSystemGroups updatedGroups groupsAndTargets
  | groupType group == Infection = do
    let groups = infectionGroups groupsAndTargets
    let updatedGroups = Map.insert (id' group) group groups
    setInfectionGroups updatedGroups groupsAndTargets

getGroupAndTarget :: (GroupType, Int) -> GroupsAndTargets -> (Maybe Group, Maybe Group)
getGroupAndTarget (t, id'') groups
  | t == ImmuneSystem = do
    let immuneGroup = Map.lookup id'' (immuneSystemGroups groups)
    let targets = immuneTargets groups
    if isNothing immuneGroup || id'' `Map.notMember` targets
      then (Nothing, Nothing)
      else (immuneGroup, Map.lookup (targets Map.! id'') (infectionGroups groups))
  | t == Infection = do
    let infectionGroup = Map.lookup id'' (infectionGroups groups)
    let targets = infectionTargets groups
    if isNothing infectionGroup || id'' `Map.notMember` targets
      then (Nothing, Nothing)
      else (infectionGroup, Map.lookup (targets Map.! id'') (immuneSystemGroups groups))

findOrderOfAttacks :: [Group] -> [(GroupType, Int)]
findOrderOfAttacks groups = map (\g -> (groupType g, id' g)) $ List.sortBy (\g1 g2 -> flip compare (initiative g1) (initiative g2)) groups

findWhoAttacksWho :: [Group] -> [Group] -> (Map.Map Int Int, Map.Map Int Int)
findWhoAttacksWho immuneGroups infectionGroups = (getAttackMatches immuneGroups infectionGroups Map.empty
                                                , getAttackMatches infectionGroups immuneGroups Map.empty)

getAttackMatches :: [Group] -> [Group] -> Map.Map Int Int -> Map.Map Int Int
getAttackMatches [] _ matches = matches
getAttackMatches _ [] matches = matches
getAttackMatches (a:as) defending matches = do
  let defenderToAttack = chooseOpposingGroupToInfect a defending
  let updatedDef = List.delete defenderToAttack defending
  getAttackMatches as updatedDef (Map.insert (id' a) (id' defenderToAttack) matches)

chooseOpposingGroupToInfect :: Group -> [Group] -> Group
chooseOpposingGroupToInfect group enemies =
    last
    $ List.sortBy (\g1 g2 -> compareDamageDealt g1 g2 group) enemies

compareDamageDealt :: Group -> Group -> Group -> Ordering
compareDamageDealt g1 g2 damageDealer
  | damageTo1 == damageTo2 = compare g1 g2
  | otherwise = compare damageTo1 damageTo2
  where damageTo1 = calculateDamage g1 damageDealer
        damageTo2 = calculateDamage g2 damageDealer

calculateDamage :: Group -> Group -> Int
calculateDamage receiver dealer
  | damageType' `elem` (weaknesses receiver) = damage * 2
  | damageType' `elem` (immunities receiver) = 0
  | otherwise = damage
  where damage= effectivePower dealer
        damageType' = damageType dealer

---------------
-- parse
---------------

parseInput raw = do
  let (immuneSystem, infection) = break (== "Infection:") $ lines raw
  let immuneSystemGroups = parseGroups immuneSystem
  let infectionGroups = parseGroups infection
  (immuneSystemGroups, infectionGroups)

parseGroups :: [String] -> [Group]
parseGroups rawGroups = do
  let groupType = parseGroupType $ head rawGroups
  map (`parseGroup` groupType) $ zip [0..] $ tail rawGroups

parseGroup :: (Int, String) -> GroupType -> Group
parseGroup (identity, rawGroup) t = do
  let theWords = words rawGroup
  let amount = parseInt $ head theWords
  let hp = parseInt $ theWords!!4
  let (weaknesses, immunities) = parseWeaknessesAndImmunities rawGroup
  let damageType = theWords!!(length theWords - 5)
  let damage = parseInt $ theWords!!(length theWords - 6)
  let initiative = parseInt $ last theWords
  Group identity t amount hp weaknesses immunities damageType damage initiative

parseWeaknessesAndImmunities raw
  | '(' `notElem` raw = ([],[])
  | otherwise = getWeakAndImm $ takeWhile (/= ')') $ dropWhile (/= '(') $ raw

getWeakAndImm raw
  | ';' `elem` raw = do
    let (imm, weak) = break (==';') raw
    (parseTypes weak, parseTypes imm)
  | take 5 raw == "(weak" = (parseTypes raw, [])
  | take 5 raw == "(immu" = ([], parseTypes raw)
  | otherwise = error raw

parseTypes raw
  | ',' `notElem` raw = [last $ words raw]
  | otherwise = do
    let types = splitOn "," raw
    map (last) $ map (words) types

parseGroupType str
  | str == "Immune System:" = ImmuneSystem
  | str == "Infection:" = Infection

data Group = Group {
  id' :: Int,
  groupType :: GroupType,
  amountOfUnits :: Int,
  hitPoints :: Int,
  weaknesses :: [DamageType],
  immunities :: [DamageType],
  damageType :: DamageType,
  damagePower :: Int,
  initiative :: Int
} deriving (Show, Eq, Read)

setAmountOfUnits amt (Group id' groupType _ hp weaknesses immunities damageType damagePower initiative) =
  Group id' groupType amt hp weaknesses immunities damageType damagePower initiative

effectivePower (Group {amountOfUnits = amt, damagePower = dmg}) = amt * dmg

data GroupsAndTargets = GroupsAndTargets {
  immuneSystemGroups :: IdsToGroups,
  infectionGroups :: IdsToGroups,
  immuneTargets :: TargetsMap,
  infectionTargets :: TargetsMap
} deriving (Show)

setImmuneSystemGroups groups (GroupsAndTargets _ infectionGroups immuneTargets infectionTargets) =
  GroupsAndTargets groups infectionGroups immuneTargets infectionTargets

setInfectionGroups groups (GroupsAndTargets immuneSystemGroups _ immuneTargets infectionTargets) =
  GroupsAndTargets immuneSystemGroups groups immuneTargets infectionTargets

instance Ord Group where
  compare group1 group2
    |  ep1 == ep2 = compare (initiative group1) (initiative group2)
    | otherwise = compare ep1 ep2
    where ep1 = effectivePower group1
          ep2 = effectivePower group2

data GroupType = ImmuneSystem | Infection deriving (Show, Eq, Read)
type DamageType = String
type IdsToGroups = Map.Map Int Group
type TargetsMap = Map.Map Int Int

parseInt str = read str :: Int
mapIdToIdentity groups = Map.fromList $ map (\g -> (id' g, g)) groups
isEmpty map = map == Map.empty