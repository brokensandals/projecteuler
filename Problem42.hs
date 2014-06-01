import Data.Char

square :: Int -> Bool
square n = (floor (sqrt (fromIntegral n)))^2 == n

letterValue :: Char -> Int
letterValue c = (ord c) - (ord 'A') + 1

wordValue :: String -> Int
wordValue s = sum $ map letterValue s

triangular :: Int -> Bool
triangular n = square (8*n + 1)

problem42 = do
  file <- readFile "words.txt"
  let filewords = read ("[" ++ file ++ "]")
  return $ length $ filter triangular $ map wordValue filewords
