-- (0.38 secs, 115116056 bytes)

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
primeFlags = sieve 10000

primes4Digits :: [Int]
primes4Digits = filter (\x -> x >= 1000 && x <= 9999) $ V.toList $ V.elemIndices True primeFlags

isPrime :: Int -> Bool
isPrime n = primeFlags ! n

isDigitalPermutation :: Int -> Int -> Bool
isDigitalPermutation x y = (sort (show x)) == (sort (show y))

problem49 = disp $ head $ concat [matches i | i <- primes4Digits,
                                              i /= 1487]
  where matches i = [(i,j,k) | j <- dropWhile (<= i) primes4Digits,
                               let k = j + (j - i),
                               k <= 9999,
                               isPrime k,
                               isDigitalPermutation i j,
                               isDigitalPermutation i k]
        disp (i,j,k) = concat $ map show [i,j,k]
