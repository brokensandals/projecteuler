import Data.List

-- Takes about a minute for me

divisible :: Integral a => a -> a -> Bool
divisible x y = (x `rem` y) == 0

primes :: Integral a => [a]
primes = 2 : primesTail 3 [2]
  where primesTail candidate ps
          | (not (any (divisible candidate) ps)) = candidate : (primesTail nextCandidate (ps ++ [candidate]))
          | otherwise                            = primesTail nextCandidate ps
          where nextCandidate = candidate + 2

problem7 = primes !! 10000
