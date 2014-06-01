champernowne = concatMap show [1..]
problem40 = product $ map (\x -> read [x]) $ map (champernowne !!) [10^i - 1 | i <- [0..6]]
