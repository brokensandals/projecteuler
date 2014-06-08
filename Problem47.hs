-- (53.46 secs, 11301984112 bytes)

import Data.Maybe
import Data.List
import Control.Monad
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

sieve :: Int -> Vector Bool
sieve n = V.modify proc base
  where base = (V.//) (V.replicate n True) [(0, False), (1, False)]
        proc v = do
          forM_ [2..(n-1)] $ \i -> do
            isprime <- MV.read v i
            when isprime $ forM_ [i*2,i*3..(n-1)] $ \j ->
              MV.write v j False

primeFlags :: Vector Bool
primeFlags = sieve 1000000

primes :: [Int]
primes = V.toList $ V.elemIndices True primeFlags

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = f : (primeFactors (n `div` f))
  where f = fromJust $ find ((== 0) . (rem n)) primes

uniqPrimeFactorCount :: Int -> Int
uniqPrimeFactorCount n = length $ nub $ primeFactors n

problem47 = head [i | i <- [2..],
                      (map uniqPrimeFactorCount [i..i+3]) == [4,4,4,4]]
