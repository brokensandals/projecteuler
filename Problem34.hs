-- 0m10.741s

import Data.List
import Data.Maybe
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec

fac0To9 :: Vector Int
fac0To9 = Vec.fromList $ map fac [0..9]
  where fac n = product [1..n]

digitFacSum :: Int -> Int
digitFacSum n = helper n 0
  where helper x total
          | x < 10 = total + (fac0To9 ! x)
          | otherwise = helper (x `div` 10) (total + (fac0To9 ! (x `rem` 10)))

curious :: Int -> Bool
curious n = n == (digitFacSum n)

limit :: Int
limit = fromJust $ find (\n -> n > (digitFacSum n)) [(10^i) - 1 | i <- [1..]]

problem34 = sum $ filter curious $ [3..limit]

main = putStrLn $ show problem34
