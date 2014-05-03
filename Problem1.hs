import Data.List

multiples :: Integral a => a -> [a]
multiples n = [n,n+n..]

-- there's got to be a more concise way to write this function
uniqMultiples :: Integral a => [a] -> [a]
uniqMultiples ns = nextStep (map multiples ns)
  where nextStep xs = nextVal : nextStep (map dropVal xs)
          where dropVal ms = dropWhile (<= nextVal) ms
                nextVal = minimum (map head xs)

problem1 = sum $ takeWhile (< 1000) $ uniqMultiples [3, 5]
