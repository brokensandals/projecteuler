import Data.List
import Data.Maybe

cOfTriple :: Integral a => (a, a) -> Maybe a
cOfTriple (a,b)
  | floor c == ceiling c = Just $ floor c
  | otherwise            = Nothing
  where c = sqrt $ fromIntegral $ a*a + b*b

tripleSum :: Integral a => (a, a) -> a
tripleSum (a,b) = a + b + c
  where c = fromJust $ cOfTriple (a,b)

tripleProd :: Integral a => (a, a) -> a
tripleProd (a,b) = a * b * c
  where c = fromJust $ cOfTriple (a,b)

problem9 = tripleProd triple1000
  where triple1000 = fromJust $ find (\x -> (tripleSum x) == 1000) candidates
        candidates = filter (isJust . cOfTriple) [(a,b) | a <- [1..1000], b <- [1..1000]]
