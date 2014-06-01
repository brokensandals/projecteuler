import Data.Maybe
import Data.List

pandigitals :: [Int]
pandigitals = reverse $ sort $ map read $ concat [permutations ['1'..n] | n <- ['1'..'9']]

prime :: Int -> Bool
prime 2 = True
prime n = odd n && (not (any ((== 0) . (rem n)) [3,5..(div n 2)]))

problem41 = fromJust $ find prime pandigitals