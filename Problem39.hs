-- (4.57 secs, 1790915336 bytes)

import Data.List
import Data.Ord

triangles :: [(Int, Int, Int)]
triangles = [(a, b, c) |  a <- [1..999],
                          b <- [1..a],
                          let cfloat = (sqrt (fromIntegral (a^2 + b^2))),
                          (floor cfloat) == (ceiling cfloat),
                          let c = floor cfloat]

perimeter :: (Int, Int, Int) -> Int
perimeter (a, b, c) = a + b + c

trianglesWithPerimeter :: Int -> [(Int, Int, Int)]
trianglesWithPerimeter p = filter ((== p) . perimeter) triangles

problem39 = maximumBy (comparing (length . trianglesWithPerimeter)) [1..1000]
