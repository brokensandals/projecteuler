import Data.List
import Data.Maybe
import Data.Ord

lencycle :: Int -> Int
lencycle d = next 10 []
  where next r rs
          | r `rem` d == 0 = 0
          | isJust dupindex = (fromJust dupindex) + 1
          | r < d = next (r * 10) (r : rs)
          | otherwise = next ((r `rem` d) * 10) (r : rs)
          where dupindex = elemIndex r rs

problem26 = maximumBy (comparing lencycle) [1..999]
