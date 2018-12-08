import Data.Map

main = do
        contents <- readFile "boxids.txt"
        let boxids = words contents
        let charCountList = Prelude.map (getCharCountsPerWord) boxids
        let doubles = countDoubles charCountList
        let trips = countTriples charCountList
        print (doubles * trips)

getCharCountsPerWord str = Prelude.map (snd) (toList (fromListWith (+) (zip str [1,1..])))

countDoubles charcountsList = countWordsWithCharOccurrence charcountsList 2

countTriples charcountsList = countWordsWithCharOccurrence charcountsList 3

countWordsWithCharOccurrence charcountsList nr = sum ([ 1 | charcounts <- charcountsList, any ( nr ==) charcounts])