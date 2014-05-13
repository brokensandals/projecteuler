digsum :: Integer -> Integer
digsum n = step n 0
  where step 0 t = t
        step x t = step (x `div` 10) (t + (x `rem` 10))

fac :: Integer -> Integer
fac n = product [1..n]

problem20 = digsum $ fac $ 100
