import Data.Bits
import Data.Set

func prevR3 prevR5 = do
    let new = (prevR3.&.255)
    let r3 = prevR3 `div` 256
    (r3, ((((prevR5 + new) .&. 16777215)* 65899) .&. 16777215))

resetR3 prev = (prev .|. 65536)
resetR5 = 7586220

evaluate:: Integer -> Integer -> Integer
evaluate prevR3 prevR5
  | prevR3 < 256 = newR5
  | otherwise = evaluate newR3 newR5
    where (newR3, newR5) = func prevR3 prevR5

startRun = runUntilDuplicateIsFound 0 0 empty

runUntilDuplicateIsFound prev prePrev results
  | prev `elem` results = prePrev
  | otherwise = runUntilDuplicateIsFound (evaluate r3 r5) prev (insert prev results)
  where r3 = resetR3 prev
        r5 = resetR5

