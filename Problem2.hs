fibSeq :: [Integer]
fibSeq = nextFibs 1 2
  where nextFibs a b = a : (nextFibs b (a+b))

problem2 = sum $ takeWhile (<= 4000000) $ filter even fibSeq
