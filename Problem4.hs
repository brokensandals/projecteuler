import Data.List

palindrome :: String => Bool
palindrome str = (take halfsize str) == (take halfsize (reverse str))
  where halfsize = quot (length str) 2

problem4 = find palindrome $ map show $ reverse $ sort $ products [100..999]
  where products []          = []
        products nums@(n:ns) = concat [(map (* n) nums), (products ns)]
