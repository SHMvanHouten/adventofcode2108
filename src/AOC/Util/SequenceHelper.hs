module AOC.Util.SequenceHelper where

import Data.Sequence

-- fails on empty sequences
lastElem :: Seq a -> a
lastElem (viewr -> xs :> x) = x