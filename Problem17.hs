text :: Integer -> String
text 0 = ""
text 1 = "one"
text 2 = "two"
text 3 = "three"
text 4 = "four"
text 5 = "five"
text 6 = "six"
text 7 = "seven"
text 8 = "eight"
text 9 = "nine"
text 10 = "ten"
text 11 = "eleven"
text 12 = "twelve"
text 13 = "thirteen"
text 14 = "fourteen"
text 15 = "fifteen"
text 16 = "sixteen"
text 17 = "seventeen"
text 18 = "eighteen"
text 19 = "nineteen"
text 20 = "twenty"
text 30 = "thirty"
text 40 = "forty"
text 50 = "fifty"
text 60 = "sixty"
text 70 = "seventy"
text 80 = "eighty"
text 90 = "ninety"
text 1000 = "onethousand"
text n
  | n < 100   = (text ((n `div` 10) * 10)) ++ (text (n `rem` 10))
  | otherwise = (text (n `div` 100)) ++ "hundred" ++ (withAnd (text (n `rem` 100)))
  where withAnd "" = ""
        withAnd x  = "and" ++ x

problem17 = length $ concatMap text [1..1000]
