-- (8.44 secs, 2259490320 bytes)

digits :: Int -> Int -> [Int]
digits base n = step n []
  where step x ds
          | x < base = x : ds
          | otherwise = step (x `div` base) ((x `rem` base) : ds)

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == (reverse xs)

problem36 = sum [n | n <- [1..999999], palindrome (digits 10 n), palindrome (digits 2 n)]
