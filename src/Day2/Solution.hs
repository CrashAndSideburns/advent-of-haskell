module Main (main) where

import Data.List

-- Puzzle input.
input :: IO [Command]
input = map parseCommand . lines <$> readFile "input/Day2/input.txt"

-- Parse input.
parseCommand :: String -> Command
parseCommand str
    | "forward" `isPrefixOf` str = Forward $ read $ drop 8 str
    | "up" `isPrefixOf` str = Up $ read $ drop 3 str
    | "down" `isPrefixOf` str = Down $ read $ drop 5 str
    | otherwise = error "Invalid command."

-- Some data types to make the solution somewhat more semantic.
type HorizontalPosition = Integer
type Depth = Integer
type Aim = Integer

data Command = Forward Integer | Up Integer | Down Integer
data Submarine = Submarine HorizontalPosition Depth Aim

-- Compute the result of applying [Command] to Submarine 0 0 0.
-- Return product of final horizontal position and depth.
-- Using foldl is necessary because Commands don't generally commute.
applyCommands :: (Submarine -> Command -> Submarine) -> [Command] -> Integer
applyCommands f = resolve . foldl f (Submarine 0 0 0)
    where resolve (Submarine h d _) = h * d

-- Solve Part 1.
solve1 :: [Command] -> Integer
solve1 = applyCommands rule
    where rule (Submarine h d _) (Forward n) = Submarine (h+n) d    0
          rule (Submarine h d _) (Up n)      = Submarine  h   (d-n) 0
          rule (Submarine h d _) (Down n)    = Submarine  h   (d+n) 0

-- Solve Part 2.
solve2 :: [Command] -> Integer
solve2 = applyCommands rule
    where rule (Submarine h d a) (Forward n) = Submarine (h+n) (d+n*a) a
          rule (Submarine h d a) (Up n)      = Submarine  h     d     (a-n)
          rule (Submarine h d a) (Down n)    = Submarine  h     d     (a+n)

-- Print solutions.
main :: IO ()
main = do input >>= print . solve1
          input >>= print . solve2
