-- 0m35.503s

import Data.List

pandigitals :: [String]
pandigitals = filter notLeadingZero $ permutations "0123456789"
  where notLeadingZero (c:cs) = c /= '0'

primes :: [Int]
primes = [2,3,5,7,11,13,17]

check :: String -> Bool
check s = all divisible $ zip [read (take 3 (drop i s)) | i <- [1..7]] primes
  where divisible (x,y) = (x `rem` y) == 0

problem43 = sum $ map read $ filter check pandigitals

main = putStrLn $ show problem43
