module Main (main) where

import Data.List
import Data.Char

-- Puzzle input.
input :: IO [BinaryNumber]
input = map (map digitToInt) . lines <$> readFile "input/Day3/input.txt"

type BinaryNumber = [Int]

-- Return the most common digit in a BinaryNumber.
mostCommonDigit :: BinaryNumber -> Int
mostCommonDigit b
    | 2 * sum b >= length b = 1
    | otherwise             = 0

-- Return the least common digit in a BinaryNumber.
leastCommonDigit :: BinaryNumber -> Int
leastCommonDigit b
    | 2 * sum b < length b = 1
    | otherwise            = 0

-- Given [BinaryNumber], return the most common digit at index Int.
mostCommonAtPosition :: [BinaryNumber] -> Int -> Int
mostCommonAtPosition bs = mostCommonDigit . (!!) (transpose bs)

-- Given [BinaryNumber], return the least common digit at index Int.
leastCommonAtPosition :: [BinaryNumber] -> Int -> Int
leastCommonAtPosition bs = leastCommonDigit . (!!) (transpose bs)

-- Return an integer representation of a BinaryNumber.
binNumToInt :: BinaryNumber -> Int
binNumToInt = sum . zipWith (*) (map (2^) [0..]) . reverse

-- Given [BinaryNumber], return the corresponding Gamma Rate.
gammaRate :: [BinaryNumber] -> Int
gammaRate = binNumToInt . map mostCommonDigit . transpose

-- Given [BinaryNumber], return the corresponding Epsilon Rate.
epsilonRate :: [BinaryNumber] -> Int
epsilonRate = binNumToInt . map leastCommonDigit . transpose

-- Given [BinaryNumber], return the corresponding Oxygen Generator Rating.
oxygenGeneratorRating :: [BinaryNumber] -> Int
oxygenGeneratorRating []  = undefined
oxygenGeneratorRating bs  = go bs 0
    where go [b] _ = binNumToInt b
          go bs n = go (filter (\b -> b!!n == mostCommonAtPosition bs n) bs) (n+1)

-- Given [BinaryNumber], return the corresponding CO2 Scrubber Rating.
co2ScrubberRating :: [BinaryNumber] -> Int
co2ScrubberRating [] = undefined
co2ScrubberRating bs = go bs 0
    where go [b] _ = binNumToInt b
          go bs n = go (filter (\b -> b!!n == leastCommonAtPosition bs n) bs) (n+1)

-- Solve Part 1.
solve1 :: [BinaryNumber] -> Int
solve1 bs = gammaRate bs * epsilonRate bs

-- Solve Part 2.
solve2 :: [BinaryNumber] -> Int
solve2 bs = oxygenGeneratorRating bs * co2ScrubberRating bs

-- Print solutions.
main :: IO ()
main = do input >>= print . solve1
          input >>= print . solve2
