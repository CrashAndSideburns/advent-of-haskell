module Main (main) where

import Data.List
import Data.List.Split

-- Representation of a line with a start and end point.
data Line = Line (Integer, Integer) (Integer, Integer)

-- Puzzle input.
input :: IO [Line]
input = map parseInput . lines <$> readFile "input/Day5/input.txt"

-- Parse input.
parseInput :: String -> Line
parseInput s = Line (parsePoint (splitString!!0)) (parsePoint (splitString!!1))
    where splitString = splitOn "->" s

-- Parse point.
parsePoint :: String -> (Integer, Integer)
parsePoint s = (read (splitString!!0), read (splitString!!1))
    where splitString = splitOn "," s

-- Given Line, return Bool representing if it is vertical or horizontal.
checkStraight :: Line -> Bool
checkStraight (Line (x1, y1) (x2, y2)) = x1 == x2 || y1 == y2

-- Given Line, return list of points [(Integer, Integer)] covered by the line.
getPoints :: Line -> [(Integer, Integer)]
getPoints (Line (x1, y1) (x2, y2)) = zip (coordRange x1 x2) (coordRange y1 y2)
    where coordRange x1 x2
              | x1 < x2 = [x1..x2]
              | x1 > x2 = reverse [x2..x1]
              | otherwise = repeat x1

-- Solve Part 1.
solve1 :: [Line] -> Int
solve1 = solve2 . filter checkStraight

-- Solve Part 2.
solve2 :: [Line] -> Int
solve2 = length . filter (>=2) . map length . group . sort . concatMap getPoints

-- Print solutions.
main :: IO ()
main = do input >>= print . solve1
          input >>= print . solve2
