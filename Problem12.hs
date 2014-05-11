import Data.List
import Data.Maybe

divisible :: Integral a => a -> a -> Bool
divisible x y = (x `rem` y) == 0

prime :: Integral a => a -> Bool
prime n = not $ any (divisible n) [2..n-1]

primes :: [Integer]
primes = filter prime [2..]

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = smallestPrimeFactor : primeFactors (n `quot` smallestPrimeFactor)
  where smallestPrimeFactor = fromJust $ find (divisible n) primes

numDivisors :: Integer -> Integer
numDivisors n = product $ map ((+ 1) . fromIntegral . length) $ group $ primeFactors n

triangulars :: [Integer]
triangulars = next 0 1 where next t v = (t+v) : (next (t+v) (v+1))

problem12 = find ((> 500) . numDivisors) triangulars
