-- compile with -O to avoid stack overflow

import Data.List
import Data.Ord

collatzlen :: Integer -> Integer
collatzlen n = step n 1
  where step 1 len = len
        step x len = step next (1 + len)
          where next
                  | even x    = x `div` 2
                  | otherwise = 3 * x + 1

problem14 = maximumBy (comparing collatzlen) [1..999999]

main = putStrLn $ show problem14
