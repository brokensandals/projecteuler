paths :: Int -> Integer
paths n = head $ foldl row bottom [n-1,n-2..0]
  where row below _ = foldl addCol [1] [n-1,n-2..0]
          where addCol row x = ((head row) + (below !! x)) : row
        bottom      = replicate n 1

problem15 = paths 20
