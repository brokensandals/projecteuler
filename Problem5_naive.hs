import Data.List

-- Naive algorithm - works for 1..10, not practical for 1..20

divisible :: Integral a => a -> a -> Bool
divisible x y = (x `rem` y) == 0

problem5 = find isMultiple [1..]
  where isMultiple n = and $ map (divisible n) [1..20]