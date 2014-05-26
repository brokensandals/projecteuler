-- (11.48 secs, 2281015264 bytes)

denominations = [200, 100, 50, 20, 10, 5, 2, 1]

nways 0 _ = 1
nways target limit
  | target < 0 = 0
  | otherwise = sum $ map nwaysafter $ filter (<= limit) denominations
  where nwaysafter coin = nways (target - coin) coin

problem31 = nways 200 200
