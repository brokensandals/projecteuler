import Data.List

prime :: Int -> Bool
prime n = not $ any ((== 0) . (rem n)) [2..n-1]

primes :: [Int]
primes = filter prime [2..]

primeFactors :: Int -> [Int]
primeFactors n = helper n primes
  where helper 1 ps = []
        helper n (p:ps)
          | (n `rem` p) == 0 = p : (helper (n `div` p) (p:ps))
          | otherwise = helper n ps

cancel :: Ord a => [a] -> [a] -> [a]
cancel [] _ = []
cancel vs [] = vs
cancel (v:vs) (c:cs)
  | v == c = cancel vs cs
  | v > c = cancel (v:vs) cs
  | otherwise = v : (cancel vs (c:cs))

reduceFrac :: (Int, Int) -> (Int, Int)
reduceFrac frac@(num, denom)
  | num == 0 || denom == 0 = frac
  | otherwise = (product (cancel numfs denomfs), product (cancel denomfs numfs))
  where numfs = primeFactors num
        denomfs = primeFactors denom

bogusFactors :: Int -> [Int]
bogusFactors n = sort $ map (\x -> read [x]) $ show n

bogusReduce :: (Int, Int) -> Maybe (Int, Int)
bogusReduce frac@(num, denom)
  | null (intersect numbfs denombfs) = Nothing
  | otherwise = Just (reduceFrac (product (cancel numbfs denombfs), product (cancel denombfs numbfs)))
  where numbfs = bogusFactors num
        denombfs = bogusFactors denom

curious :: (Int, Int) -> Bool
curious frac@(num, denom) = helper (bogusReduce frac)
  where helper Nothing = False
        helper (Just bogus) = (reduceFrac frac) == bogus

fracProd :: (Int, Int) -> (Int, Int) -> (Int, Int)
fracProd (n1, d1) (n2, d2) = (n1*n2, d1*d2)

problem33 = reduceFrac $ foldl1 fracProd [(a,b) | a <- [10..99], b <- [10..99], a < b, nontrivial (a,b), curious (a,b)]
  where nontrivial (a,b) = (a `rem` 10) /= 0 || (b `rem` 10) /= 0
