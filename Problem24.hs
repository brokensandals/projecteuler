import Data.Vector (Vector, (//), (!))
import qualified Data.Vector as V

fac :: Integral a => a -> a
fac n = product [1..n]

factoradic :: Integral a => a -> Int -> Vector a
factoradic 0 p = V.replicate p 0
factoradic n p = V.fromList $ reverse $ next n (p-1) []
  where next _ 0 acc = 0 : acc
        next 0 place acc = next 0 (place-1) (0 : acc)
        next cur place acc = next (rem cur pf) (place-1) ((div cur pf) : acc)
          where pf = fac (fromIntegral place)

decodeLehmer :: Integral a => Vector a -> Vector a
decodeLehmer v = V.ifoldr' next v v
  where next i x r = r // [(j, nextval (r ! j)) | j <- [i+1..(V.length r)-1]]
          where nextval cur
                  | cur >= x = cur + 1
                  | otherwise = cur

problem24 = concat $ map show $ V.toList $ decodeLehmer $ factoradic 999999 10
