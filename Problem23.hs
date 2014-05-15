import Control.Monad
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

sumsOfDivisors :: Vector Int
sumsOfDivisors = V.modify proc (V.replicate 28124 0)
  where proc v = do
          forM_ [1..28123] $ \d ->
            forM_ [d*2,d*3..28123] $ \m -> do
              cur <- MV.read v m
              MV.write v m (d + cur)

abundants :: [Int]
abundants = filter (\x -> (sumsOfDivisors ! x) > x) [1..28123]

notSumsOfAbundants :: [Int]
notSumsOfAbundants = filter (\x -> not (areSums ! x)) [1..28123]
  where areSums = V.modify proc (V.replicate 28124 False)
        proc v = do
          forM_ (wrap abundants) $ \(x:xs) ->
            forM_ (x:xs) $ \y ->
              when (x+y <= 28123) $ MV.write v (x+y) True
        wrap [] = []
        wrap (x:xs) = (x:xs) : (wrap xs)

problem23 = sum notSumsOfAbundants
