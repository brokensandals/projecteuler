-- (10.79 secs, 3077545128 bytes)

import Data.Set (Set)
import qualified Data.Set as Set

nthPentagonal :: Int -> Int
nthPentagonal n = (n * (3 * n - 1)) `div` 2

pentagonalList :: [Int]
pentagonalList = map nthPentagonal [1..1000000]

pentagonalSet :: Set Int
pentagonalSet = Set.fromList pentagonalList

isPentagonal :: Int -> Bool
isPentagonal n | n <= nthPentagonal 1000000 = Set.member n pentagonalSet

problem44 = minimum $ head $ dropWhile null $ map candidates pentagonalList
  where candidates pk = [d | pj <- takeWhile (< pk) pentagonalList,
                             let d = pk - pj,
                             isPentagonal (pj + pk),
                             isPentagonal d]
