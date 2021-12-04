module Main (main) where

import Data.List.Split
import Data.List
import Data.Maybe
import Control.Monad

-- Data types for representing games of Bingo. There's a bit of a trick here
-- in the Maybe Integer may be used to represent a square, since the integer
-- value of a square only matters before it has been crossed off.
-- Game holds an Integer representing the last number drawn to simplify
-- the calculation of Board scores.
data Game = Game Integer [Integer] [Board]
type Board = [[Square]]
type Square = Maybe Integer

-- Puzzle input.
input :: IO Game
input = parseInput . splitOn "\n\n" <$> readFile "input/Day4/input.txt"

-- Parse input.
parseInput :: [String] -> Game
parseInput (x:xs) = Game 0 (parseDraws x) (map (parseBoard . lines) xs)
parseInput _ = error "Invalid input."

-- Given String like below example, return corresponding [Integer] of draws.
--
-- "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
parseDraws :: String -> [Integer]
parseDraws = map read . splitOn ","

-- Given [String] like below example, return corresponding Board.
--
-- "22 13 17 11  0"
-- " 8  2 23  4 24"
-- "21  9 14 16  7"
-- " 6 10  3 18  5"
-- " 1 12 20 15 19"
parseBoard :: [String] -> Board
parseBoard = map $ map (return . read) . words

-- Given Board, return Bool representing whether or not Board has won.
won :: Board -> Bool
won b = rowCheck b || colCheck b
    where rowCheck = any (all isNothing)
          colCheck = rowCheck . transpose

-- Given Integer and Board, return Board with Integer-valued Squares removed.
markBoard :: Integer -> Board -> Board
markBoard n = map (map (mfilter (/=n)))

-- Given Game, return Game after one turn has passed.
takeTurn :: Game -> Game
takeTurn (Game _ [] bs) = undefined
takeTurn (Game _ (n:ns) bs) = Game n ns (map (markBoard n) bs)

-- Given Board and Integer, return Integer corresponding to the board's score.
boardScore :: Board -> Integer -> Integer
boardScore b n = n * sum (map (fromMaybe 0) $ concat b)

-- Given Game, return Integer representing winning Board's score if one exists.
wonBoardScore :: Game -> Maybe Integer
wonBoardScore (Game n _ bs) = (`boardScore` n) <$> find won bs

-- Solve Part 1.
solve1 :: Game -> Integer
solve1 = fromJust . msum . map wonBoardScore . iterate takeTurn

-- Solve Part 2.
solve2 :: Game -> Integer
solve2 = solve1 . head . filter oneBoardLeft . map removeWon . iterate takeTurn
    where oneBoardLeft (Game _ _ bs) = length bs == 1
          removeWon (Game n ns bs) = Game n ns (filter (not . won) bs)

-- Print solutions.
main :: IO ()
main = do input >>= print . solve1
          input >>= print . solve2
