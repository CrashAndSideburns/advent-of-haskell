module Main (main) where

import Data.List
import Data.List.Split

-- Puzzle input.
input :: IO [Integer]
input = parseInput <$> readFile "input/Day7/input.txt"

-- Parse input.
parseInput :: String -> [Integer]
parseInput = map read . splitOn ","

-- Given (Ord a) => [a], return the median value of the list.
median :: (Ord a) => [a] -> a
median xs = sort xs !! (length xs `div` 2)

-- Given Integer dest. and [Integer] of positions, return the part 1 fuel cost.
fuelCostOne :: [Integer] -> Integer -> Integer
fuelCostOne xs d = sum $ map (abs . subtract d) xs

-- Given Integer dest. and [Integer] of positions, return the part 2 fuel cost.
fuelCostTwo :: [Integer] -> Integer -> Integer
fuelCostTwo xs d = sum $ map (\x -> ((x - d)^2 + abs (x - d)) `div` 2) xs

-- Solve Part 1.
solve1 :: [Integer] -> Integer
solve1 xs = fuelCostOne xs (median xs)

-- Solve Part 2.
solve2 :: [Integer] -> Integer
solve2 xs = minimum $ map (fuelCostTwo xs) [minimum xs..maximum xs]

-- Print solutions.
main :: IO ()
main = do input >>= print . solve1
          input >>= print . solve2
