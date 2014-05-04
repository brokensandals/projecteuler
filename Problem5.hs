import Data.List
import Data.Maybe

divisible :: Integral a => a -> a -> Bool
divisible x y = (x `rem` y) == 0

prime :: Integral a => a -> Bool
prime n = not $ any (divisible n) [2..n-1]

primes :: Integral a => [a]
primes = filter prime [2..]

primeFactors :: Integral a => a -> [a]
primeFactors n
  | prime n = [n]
  | otherwise = smallestPrimeFactor : primeFactors (n `quot` smallestPrimeFactor)
      where smallestPrimeFactor = fromJust $ find (divisible n) primes

occurrences :: Eq a => a -> [a] -> Int
occurrences x xs = length $ elemIndices x xs

problem5 = foldl1 (*) lcmFactors
  where factorLists       = map primeFactors [1..20]
        uniqFactors       = nub $ concat factorLists
        replicateFactor f = replicate (maximum $ map (occurrences f) factorLists) f
        lcmFactors        = concat $ map replicateFactor uniqFactors
