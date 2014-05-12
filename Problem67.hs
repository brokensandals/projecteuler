import Data.List

maxTotal :: [[Integer]] -> Integer
maxTotal rows = head $ foldl next zeros rows
  where zeros = replicate ((length (head rows)) + 1) 0
        next prev cur = zipWith best cur [0..(length cur)-1]
          where best v i = max (v + (prev !! i)) (v + (prev !! (i+1)))

split :: Char -> String -> [String]
split sep str = filter (/= [sep]) $ groupBy f str
  where f a b = a /= sep && b /= sep

parseTriangle :: String -> [[Integer]]
parseTriangle str = reverse $ map row $ split '\n' str
  where row r = map read $ split ' ' r

problem19 = do
    file <- readFile "triangle.txt"
    return $ maxTotal $ parseTriangle file
