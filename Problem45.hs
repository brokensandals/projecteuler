-- (0.28 secs, 70010528 bytes)

import Data.List

triangulars :: [Int]
triangulars = map (\n -> (n*(n+1)) `div` 2) [1..]

pentagonals :: [Int]
pentagonals = map (\n -> (n*(3*n - 1)) `div` 2) [1..]

hexagonals :: [Int]
hexagonals = map (\n -> (n*(2*n - 1))) [1..]

sortedIntersect :: Ord a => [a] -> [a] -> [a]
sortedIntersect [] _ = []
sortedIntersect _ [] = []
sortedIntersect (x:xs) (y:ys)
  | x == y = x : (sortedIntersect xs ys)
  | x < y = sortedIntersect (dropWhile (< y) xs) (y:ys)
  | x > y = sortedIntersect (x:xs) (dropWhile (< x) ys)

triPentHexals :: [Int]
triPentHexals = sortedIntersect triangulars $ sortedIntersect pentagonals hexagonals

problem45 = find (> 40755) triPentHexals
