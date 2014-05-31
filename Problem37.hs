-- (3.38 secs, 1703155832 bytes)

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

truncatable :: Int -> Bool
truncatable p = helper (p `div` 10) 10 (p `rem` 10)
  where helper :: Int -> Int -> Int -> Bool
        helper n t s
          | n < 10 = passes
          | otherwise = passes && (helper (n `div` 10) (t*10) (s + (n `rem` 10)*t))
          where passes :: Bool
                passes = (primeFlags ! n) && (primeFlags ! s)

problem37 = sum $ filter truncatable primes
