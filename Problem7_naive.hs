import Data.List

-- Works for 6th, slow for 10001th

divisible :: Integral a => a -> a -> Bool
divisible x y = (x `rem` y) == 0

prime :: Integral a => a -> Bool
prime n = not $ any (divisible n) [2..n-1]

primes :: Integral a => [a]
primes = filter prime [2..]

problem7 = primes !! 10000
