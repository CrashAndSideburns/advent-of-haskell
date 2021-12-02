module Day1.Solution where

-- The actual puzzle input.
input :: IO [Integer]
input = map read . lines <$> readFile "input.txt"

-- Return the number of elements in a list greater than their predecessor.
increases :: (Ord a) => [a] -> Integer
increases [] = 0
increases (_:[]) = 0
increases (x1:x2:xs)
    | x2 > x1   = increases (x2:xs) + 1
    | otherwise = increases (x2:xs)

-- Group a list into 3-tuples of adjacent elements.
group :: [a] -> [(a, a, a)]
group [] = []
group (_:[]) = []
group (_:_:[]) = []
group (x1:x2:x3:xs) = (x1, x2, x3) : (group $ (x2:x3:xs))

-- Solve part 1.
solve1 :: [Integer] -> Integer
solve1 = increases

-- Solve part 2.
solve2 :: [Integer] -> Integer
solve2 = increases . map (\(x, y, z) -> x + y + z) . group

-- Print solutions.
main :: IO ()
main = do input >>= print . solve1
          input >>= print . solve2
