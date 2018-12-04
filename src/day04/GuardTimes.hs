import Data.List.Split
import Data.DateTime
import Data.List
import Data.Map
-- checked: no sleep before 00:00

main = do
        contents <- readFile "inputDay4.txt"
        let records = sortOn (fst) (Prelude.map (toRecord) (Prelude.map words (lines contents)))
        let guards = toGuards (Prelude.map (snd) records) [] [] 0
        let timeSleptByGuards = Prelude.map (\x -> (x, getTotalTimeSlept x)) guards
        let sortedGuards = sortOn (snd) timeSleptByGuards
        let sleepyGuard = fst (tail sortedGuards)

        print "hi"
---------
----Guard stats
---------
getTotalTimeSlept:: Guard -> Int
getTotalTimeSlept guard = sum (elems (sleepMinutesToAmount guard))


----------
----Guards
----------
data Guard = Guard {
  id::Int,
  sleepMinutesToAmount::Map Int Int
} deriving (Show)

data SleepPeriod = SleepPeriod {
  sleepMin::Int,
  wakeMin::Int
} deriving (Show)

toGuards :: [Record] -> [Guard] -> [SleepPeriod] -> Int -> [Guard]
toGuards [x] guards sleepPeriods id = (toGuard sleepPeriods id):guards
toGuards (x:xs) guards sleepPeriods id
   | (recordType x == GuardRecord) = toGuards xs ((toGuard sleepPeriods id):guards) [] (identity x)
   | (recordType x == SleepRecord) = toGuards xs guards ((toSleepTime x):sleepPeriods) id
   | (recordType x == WakeRecord) = toGuards xs guards (updateSleepPeriods sleepPeriods x) id

toGuard :: [SleepPeriod] -> Int -> Guard
toGuard sleepPeriods id = Guard id (fromListWith (+) (flatSleepPeriodsToMinutePairs sleepPeriods []))

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
              let id = parseInt (tail(raw!!3))
              (dateTime, Record GuardRecord (buildHourOfDay (raw!!1)) id)

toSleepRecord :: [String] -> (DateTime, Record)
toSleepRecord raw = do
              let dateTime = getDateTime raw
              (dateTime, Record SleepRecord (buildHourOfDay (raw!!1)) 0)

toWakeRecord :: [String] -> (DateTime, Record)
toWakeRecord raw = do
              let dateTime = getDateTime raw
              let id = parseInt (init(raw!!3))
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