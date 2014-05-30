-- 0m2.108s

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

primes1M :: Vector Bool
primes1M = sieve 1000000

digits :: Int -> [Int]
digits n = step n []
  where step x ds
          | x < 10 = x : ds
          | otherwise = step (x `div` 10) ((x `rem` 10) : ds)

fromDigits :: [Int] -> Int
fromDigits ds = snd $ foldr helper (1,0) ds
  where helper d (p, t) = (p*10, p*d + t)

rotations :: [a] -> [[a]]
rotations xs = helper xs (length xs)
  where helper _ 1 = []
        helper (y:ys) n = rot : (helper rot (n-1))
          where rot = ys ++ [y]

circular :: Int -> Bool
circular n = all (primes1M !) (n : (map fromDigits (rotations (digits n))))

problem35 = length $ filter circular [2..999999]

main = putStrLn $ show problem35
