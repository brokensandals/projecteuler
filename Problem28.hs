-- (0.01 secs, 2092056 bytes)
problem28 = next 1 1 2
  where next :: Int -> Int -> Int -> Int
        next total _ 1002 = total
        next total corner dist = next (total + (sum corners)) (last corners) (dist + 2)
          where corners = map (\x -> corner + x*dist) [1..4]
