digits :: Integer -> [Integer]
digits n = step n []
  where step x ds
          | x < 10 = x : ds
          | otherwise = step (x `div` 10) ((x `rem` 10) : ds)

problem16 = sum $ digits $ 2^1000
