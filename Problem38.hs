-- (3.49 secs, 4324193016 bytes)

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

pandigitals9 :: Set Int
pandigitals9 = Set.fromList $ map read $ permutations "123456789"

powersOf10 :: [Int]
powersOf10 = [10^i | i <- [1..]]

powerOf10Above :: Int -> Int
powerOf10Above n = fromJust $ find (> n) powersOf10

catDigits :: Int -> Int -> Int
catDigits a b = b + (a * (powerOf10Above b))

catSuccProds :: Int -> [Int]
catSuccProds n = scanl1 catDigits [n*i | i <- [1..]]

pandigital9SuccProds :: Int -> [Int]
pandigital9SuccProds n = filter ((flip Set.member) pandigitals9) $ takeWhile (<= 987654321) $ drop 1 $ catSuccProds n

problem38 = maximum $ concatMap pandigital9SuccProds $ takeWhile candidate [1..]
  where candidate n = ((catSuccProds n) !! 1) <= 987654321
