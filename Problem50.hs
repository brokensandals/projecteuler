-- (6.01 secs, 2167468960 bytes)

import Data.Ord
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

limit = 1000000

primeFlags :: Vector Bool
primeFlags = sieve limit

primes :: Vector Int
primes = V.elemIndices True primeFlags

primeslen = V.length primes

problem50 = fst $ considerStart (-1, 0) 0
  where considerStart best startIndex
          | startIndex == primeslen = best
          | otherwise = considerLength best startIndex 0
          where considerLength best@(bestPrime, bestLength) curIndex curSum
                  | (curIndex == primeslen) || (newSum >= limit) = considerStart best (startIndex + 1)
                  | otherwise = considerLength newBest (curIndex + 1) newSum
                  where newLength = curIndex - startIndex + 1
                        newSum = curSum + (primes ! curIndex)
                        newBest
                          | (primeFlags ! newSum) && (newLength > bestLength) = (newSum, newLength)
                          | otherwise = best

-- A naive solution that's too slow for 1000000
--problem50 = fst $ maximumBy (comparing snd) $ [(p, len) | i <- [0..(V.length primes)],
--                                                          len <- [1..(V.length primes) - i],
--                                                          let p = V.sum $ V.slice i len primes,
--                                                          p < limit,
--                                                          primeFlags ! p]

-- A silly solution that's too slow for 1000000
--problem50 = (!) primes $ snd $ considerPrime (0, -1) 0
--  where considerPrime :: (Int, Int) -> Int -> (Int, Int)
--        considerPrime best curPrimeIndex
--          | curPrimeIndex == V.length primes = best
--          | otherwise = considerStart best 0
--          where curPrime = primes ! curPrimeIndex
--                considerStart :: (Int, Int) -> Int -> (Int, Int)
--                considerStart best@(bestLength, bestLengthIndex) curStartIndex
--                  | (curStartIndex + bestLength) >= curPrimeIndex = considerPrime best (curPrimeIndex + 1)
--                  | otherwise = considerLength best (V.sum (V.slice curStartIndex bestLength primes)) bestLength
--                  where considerLength :: (Int, Int) -> Int -> Int -> (Int, Int)
--                        considerLength best@(bestLength, bestLengthIndex) curSum curLength
--                          | curSum == curPrime = considerPrime (curLength, curPrimeIndex) (curPrimeIndex + 1)
--                          | curSum > curPrime = considerStart best (curStartIndex + 1)
--                          | otherwise = considerLength best (curSum + (primes ! (curStartIndex + curLength))) (curLength + 1)
