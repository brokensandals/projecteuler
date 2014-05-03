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

problem3 = maximum (primeFactors 600851475143)
