square :: Num a => a -> a
square n = n * n

sumOfSquares :: Num a => [a] -> a
sumOfSquares ns = sum $ map square ns

squareOfSums :: Num a => [a] -> a
squareOfSums ns = square $ sum ns

problem6 = (squareOfSums ns) - (sumOfSquares ns)
  where ns = [1..100]
