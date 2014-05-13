import Data.List

leapYear :: Integer -> Bool
leapYear y
  | (y `rem` 100 == 0) = y `rem` 400 == 0
  | otherwise          = y `rem` 4 == 0

monthDays :: Integer -> Integer -> Integer
monthDays y m
  | m == 1 && leapYear y                  = 29
  | m == 1                                = 28
  | m == 3 || m == 5 || m == 8 || m == 10 = 30
  | otherwise                             = 31

-- struct for storing year num (1900-), month num (0-11), day of month (0-30), day of week (0-6, starting Monday)
data Day = Day Integer Integer Integer Integer deriving Show

nextDay :: Day -> Day
nextDay (Day y m d w) = Day ny nm nd nw
  where nw = (w+1) `rem` 7
        nd
          | changeMonth = 0
          | otherwise   = d + 1
        nm
          | changeYear  = 0
          | changeMonth = m + 1
          | otherwise   = m
        ny
          | changeYear = y + 1
          | otherwise  = y
        changeMonth = d == (monthDays y m) - 1
        changeYear = changeMonth && m == 11

days :: [Day]
days = iterate nextDay (Day 1900 0 0 0)

problem19 = length $ filter matches $ takeWhile (beforeyear 2001) $ dropWhile (beforeyear 1901) days
  where beforeyear x (Day y _ _ _) = y < x
        matches (Day _ _ d w) = w == 6 && d == 0
