import Data.List
import Data.Maybe

fibSeq :: [(Integer, Integer)]
fibSeq = nextFibs 1 1 1
  where nextFibs i a b = (i, a) : (nextFibs (i+1) b (a+b))

problem25 = fst $ fromJust $ (find ((>= (10^999)) . snd) fibSeq)
