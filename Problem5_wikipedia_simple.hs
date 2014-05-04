import Prelude hiding (all, foldl1, minimum)
import Data.Foldable
import Data.Maybe
import Data.Sequence

-- The 'simple algorithm' suggested on wikipedia - use an array of the factors and increment them
-- until they're all equal
-- Again, too slow for 1..20

problem5 = index (nextStep (fromList [1..20]) (fromList [1..20])) 0
  where nextStep ns orig
          | all (== (index ns 0)) ns = ns
          | otherwise       = nextStep (update minIndex (minVal + (index orig minIndex)) ns) orig
              where minIndex = fromJust $ elemIndexL minVal ns
                    minVal   = minimum ns