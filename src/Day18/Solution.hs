module Main (main) where

-- Puzzle input.
input :: IO [a]
input = map parseInput . lines <$> readFile "input/Dayx/input.txt"

-- Parse input.
parseInput :: String -> a
parseInput = undefined

-- Solve Part 1.
solve1 :: a -> Integer
solve1 = undefined

-- Solve Part 2.
solve2 :: a -> Integer
solve2 = undefined

-- Print solutions.
main :: IO ()
main = do input >>= print . solve1
          input >>= print . solve2
