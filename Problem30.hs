import Data.List
import Data.Maybe

digits :: Int -> [Int]
digits n
  | n < 10 = [n]
  | otherwise = (n `rem` 10) : (digits (n `div` 10))

digpowsum :: Int -> Int -> Int
digpowsum p n = sum $ map (^ p) $ digits n

matches :: Int -> [Int]
matches p = filter eqsum [2..maxcandidate]
  where eqsum x = x == digpowsum p x
        maxcandidate = fromJust $ find (\x -> 9^p * (length (digits x)) < x) $ iterate (* 10) 1

problem30 = sum $ matches 5
