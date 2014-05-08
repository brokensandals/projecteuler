import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- Takes around 50 seconds for me

sieve :: Int -> V.Vector Bool
sieve n = V.modify (walk 2) base
  where base = (V.//) (V.replicate n False) [(0, True), (1, True)]
        walk next v
          | next < n  = do
              mark (next+next) next v
              walk (next+1) v
          | otherwise = return ()
        mark next step v
          | next < n  = do
              MV.write v next True
              mark (next+step) step v
          | otherwise = return ()

primesBelow :: Int -> [Int]
primesBelow n = V.toList $ V.elemIndices False $ sieve n

problem10 = sum $ primesBelow 2000000
