import Data.List.Split
import Data.DateTime
import Data.List
import Data.Map
-- checked: no sleep before 00:00

main = do
        contents <- readFile "inputDay4.txt"
        let records = sortOn (fst) (Prelude.map (toRecord) (Prelude.map words (lines contents)))
        let guards = toGuards (Prelude.map (snd) records) Data.Map.empty [] 0

        ------ laziest guard
        let timeSleptByGuards = Prelude.map (\x -> (x, getTotalTimeSlept x)) (elems guards)
        let sortedGuards = sortOn (snd) timeSleptByGuards
        let sleepyGuard = fst (last sortedGuards)
        let sleepyGuardId = iden sleepyGuard
        let sortedByLongestSlept = sortOn snd (toList (sleepMinutesToAmount sleepyGuard))
        print "Guard id of laziest guard * longest slept minute"
        print (sleepyGuardId * (fst(last sortedByLongestSlept)))
        ------

        let guardByLongestSleptMinute = Prelude.map (\g -> (getMostSleptMinuteAmount g, g)) (elems guards)
        let guardsByLongestSleptMinuteSorted = sortOn (fst)
        print "hi"

---------
----Guard stats
---------
getTotalTimeSlept:: Guard -> Int
getTotalTimeSlept guard = sum (elems (sleepMinutesToAmount guard))

getMostSleptMinuteAmount :: Guard -> Int
--todo: how to maximumby
getMostSleptMinuteAmount guard = snd (last (sortOn (snd) (toList (sleepMinutesToAmount guard))))

----------
----Guards
----------
data Guard = Guard {
  iden::Int,
  sleepMinutesToAmount::Map Int Int
} deriving (Show)

data SleepPeriod = SleepPeriod {
  sleepMin::Int,
  wakeMin::Int
} deriving (Show)

toGuards :: [Record] -> Map Int Guard -> [SleepPeriod] -> Int -> Map Int Guard
toGuards [x] guards sleepPeriods iden = Data.Map.insert iden (toGuard sleepPeriods iden) guards
toGuards (x:xs) guards sleepPeriods iden
   | (recordType x == GuardRecord) = toGuards xs (updateGuards sleepPeriods iden guards) [] (identity x)
   | (recordType x == SleepRecord) = toGuards xs guards ((toSleepTime x):sleepPeriods) iden
   | (recordType x == WakeRecord) = toGuards xs guards (updateSleepPeriods sleepPeriods x) iden

updateGuards::[SleepPeriod] -> Int -> Map Int Guard -> Map Int Guard
updateGuards sleepPeriods iden guards
    | iden `member` guards = Data.Map.insert iden (updateGuard (guards ! iden) sleepPeriods iden) guards
    | otherwise = Data.Map.insert iden (toGuard sleepPeriods iden) guards

updateGuard::Guard -> [SleepPeriod] -> Int -> Guard
updateGuard guard sleepPeriods iden = Guard iden (unionWith (+) (sleepMinutesToAmount guard) (fromListWith (+) (flatSleepPeriodsToMinutePairs sleepPeriods [])))

toGuard :: [SleepPeriod] -> Int -> Guard
toGuard sleepPeriods iden = Guard iden (fromListWith (+) (flatSleepPeriodsToMinutePairs sleepPeriods []))

toSleepTime::Record -> SleepPeriod
toSleepTime sleepRecord = SleepPeriod (minute (timeOfDay sleepRecord)) 0

updateSleepPeriods :: [SleepPeriod] -> Record -> [SleepPeriod]
updateSleepPeriods (x:xs) wakeRecord = (SleepPeriod (sleepMin x) (minute (timeOfDay wakeRecord))) : xs

flatSleepPeriodsToMinutePairs :: [SleepPeriod] -> [(Int, Int)] -> [(Int, Int)]
flatSleepPeriodsToMinutePairs [] minutePairs = minutePairs
flatSleepPeriodsToMinutePairs [x] minutePairs = minutePairs ++ (buildMinutePairs x )
flatSleepPeriodsToMinutePairs (x:xs) minutePairs = flatSleepPeriodsToMinutePairs xs (minutePairs ++ (buildMinutePairs x ))

buildMinutePairs:: SleepPeriod -> [(Int, Int)]
buildMinutePairs period = [(x,1) | x <- [(sleepMin period)..((wakeMin period) - 1)]]

------------
---- RECORDS
------------
data TimeOfDay = TimeOfDay {
  hour::Int,
  minute::Int
} deriving (Show)

data Record = Record {
  recordType::RecordType,
  timeOfDay::TimeOfDay,
  identity::Int
} deriving (Show)

data RecordType = GuardRecord | SleepRecord | WakeRecord deriving (Show, Eq)

toRecord :: [String] -> (DateTime, Record)
toRecord rawRecord
                | identifier == "Guard" = toGuardRecord rawRecord
                | identifier == "falls" = toSleepRecord rawRecord
                | identifier == "wakes" = toWakeRecord rawRecord
                where identifier = rawRecord!!2

toGuardRecord :: [String] -> (DateTime, Record)
toGuardRecord raw = do
              let dateTime = getDateTime raw
              let iden = parseInt (tail(raw!!3))
              (dateTime, Record GuardRecord (buildHourOfDay (raw!!1)) iden)

toSleepRecord :: [String] -> (DateTime, Record)
toSleepRecord raw = do
              let dateTime = getDateTime raw
              (dateTime, Record SleepRecord (buildHourOfDay (raw!!1)) 0)

toWakeRecord :: [String] -> (DateTime, Record)
toWakeRecord raw = do
              let dateTime = getDateTime raw
              let iden = parseInt (init(raw!!3))
              (dateTime, Record WakeRecord (buildHourOfDay (raw!!1)) 0)

getDateTime:: [String] -> DateTime
getDateTime raw = do
                 let date = splitOn "-" (tail (raw!!0))
                 let time = splitOn ":" (init (raw!!1))
                 fromGregorian (parseInteger (date!!0)) (parseInt (date!!1)) (parseInt (date!!2)) (parseInt(time!!0)) (parseInt(time!!1)) 0

buildHourOfDay :: String -> TimeOfDay
buildHourOfDay raw = do
                let time = splitOn ":" (init raw)
                TimeOfDay (parseInt(time!!0)) (parseInt(time!!1))
--------
----UTIL
--------
parseInt str = read str :: Int
parseInteger str = read str :: Integer