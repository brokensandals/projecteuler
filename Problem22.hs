import Data.List
import Text.Regex

commaRegex = mkRegex ","
quoteRegex = mkRegex "\\\""

parse :: String -> [String]
parse str = map rmQuotes $ splitRegex commaRegex str
  where rmQuotes s = subRegex quoteRegex s ""

sumChars :: String -> Integer
sumChars str = sum $ map value str
  where value c = fromIntegral $ (fromEnum c) - (fromEnum 'A') + 1

score :: (Integer, String) -> Integer
score (i,str) = i * (sumChars str)

problem22 = do
  file <- readFile "names.txt"
  return $ sum $ map score $ zip [1..] $ sort $ parse file
