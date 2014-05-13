-- (non-thread-safe) memoization with help from http://augustss.blogspot.com/2011/04/ugly-memoization-heres-problem-that-i.html
import Data.IORef
import qualified Data.Map as M
import System.IO.Unsafe
import Data.List
import Data.Ord

memo :: Ord a => (a -> b) -> (a -> b)
memo f = memoized
  where ref = unsafePerformIO $ newIORef M.empty
        memoized x = unsafePerformIO $ do
          cache <- readIORef ref
          case M.lookup x cache of
            Just result -> return result
            Nothing     -> do
              let result = f x
              writeIORef ref (M.insert x result cache)
              return result


properDivisors :: Integer -> [Integer]
properDivisors n = filter (\x -> (n `rem` x) == 0) [1..(n `div` 2)]

properDivisorSum :: Integer -> Integer
properDivisorSum = memo (sum . properDivisors)

amicable :: Integer -> Bool
amicable n = (ds /= n) && (properDivisorSum ds == n)
  where ds = properDivisorSum n

problem21 = sum $ filter amicable [1..9999]
