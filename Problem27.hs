-- (39.46 secs, 12311801240 bytes)

import Control.Monad
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Maybe
import Data.List
import Data.Ord

sieve :: Int -> V.Vector Bool
sieve n = V.modify proc base
  where base = (V.//) (V.replicate n True) [(0, False), (1, False)]
        proc v = do
          forM_ [2..(n-1)] $ \i -> do
            isprime <- MV.read v i
            when isprime $ forM_ [i*2,i*3..(n-1)] $ \j ->
              MV.write v j False

-- kinda cheap, admittedly
sieve3M = sieve 3000000

prime :: Int -> Bool
prime n = n > 0 && sieve3M ! n

quadratic :: Int -> Int -> Int -> Int
quadratic a b n = n*n + n*a + b

nprimes :: Int -> Int -> Int
nprimes a b = fromIntegral $ fromJust $ findIndex (not . prime . (quadratic a b)) [0..]

-- Weirdly, (maximumBy (comparing nprimes)) is dramatically slower than this
problem27 = prod $ maximumBy (comparing (\(_,_,x) -> x)) [(a, b, nprimes a b) | a <- [-999..999], b <- [-999..999]]
  where prod (a,b,_) = a*b
