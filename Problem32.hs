-- (106.99 secs, 91153221800 bytes)

import Data.List

digprod :: String -> String -> Int
digprod a b = (read a) * (read b)

multiplicands :: [String]
multiplicands = filter correctlen $ concatMap permutations $ subsequences "123456789"
  where correctlen x = (length x) >= 1 && (length x) <= 7

multipliers :: String -> [String]
multipliers digs = filter correctlen $ concatMap permutations $ subsequences digs
  where correctlen x = (length x) >= 1 && (length x) <= maxlen
        maxlen = (length digs) - 1

unused :: String -> String
unused used = foldl (flip delete) "123456789" used

products :: [Int]
products = map snd $ filter matches $ concatMap forMultiplicand multiplicands
  where forMultiplicand :: String -> [(String, Int)]
        forMultiplicand m = map prod $ multipliers $ unused m
          where prod :: String -> (String, Int)
                prod m2 = ((unused (m ++ m2)), digprod m m2)
        matches :: (String, Int) -> Bool
        matches (digits, value) = (sort digits) == (sort (show value))

problem32 = sum $ nub $ products
