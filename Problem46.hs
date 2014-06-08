-- (3.52 secs, 1710059080 bytes)

import Data.Set (Set)
import qualified Data.Set as Set
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

limit = last primes

squares :: Set Int
squares = Set.fromList $ takeWhile (<= limit) $ map (^2) [1..]

square :: Int -> Bool
square n = Set.member n squares

goldbach :: Int -> Bool
goldbach n = any square [(n - p) `div` 2 | p <- takeWhile (< n) primes,
                                           p /= 2]

problem46 = find (not . goldbach) [n | n <- [9,11..limit], not (primeFlags ! n)]
