module Main (main) where

import Data.Either
import Data.List

data Parser = Parser String String

-- Puzzle input.
input :: IO [Parser]
input = map makeParser . lines <$> readFile "input/Day10/input.txt"

-- Given a closing parenthesis, return the corresponding opening parenthesis.
inverseParen :: Char -> Char
inverseParen ')' = '('
inverseParen ']' = '['
inverseParen '}' = '{'
inverseParen '>' = '<'
inverseParen _ = undefined

-- Given a Parser, return String if Parser is incomplete, Char if corrupt.
parseChunks :: Parser -> Either String Char
parseChunks (Parser [] ss) = Left ss
parseChunks (Parser (i:is) []) = parseChunks $ Parser is [i]
parseChunks (Parser (i:is) (s:ss))
    | i `elem` "([{<" = parseChunks $ Parser is (i:s:ss)
    | otherwise       = if s == inverseParen i
                        then parseChunks $ Parser is ss
                        else Right i

-- Return a Parser for some String.
makeParser :: String -> Parser
makeParser s = Parser s []

-- Return the score for a corrput chunk.
scoreCorrupt :: Char -> Integer
scoreCorrupt ')' = 3
scoreCorrupt ']' = 57
scoreCorrupt '}' = 1197
scoreCorrupt '>' = 25137
scoreCorrupt _ = undefined

-- Return the score for a character in an incomplete chunk.
scoreIncomplete :: Char -> Integer
scoreIncomplete '(' = 1
scoreIncomplete '[' = 2
scoreIncomplete '{' = 3
scoreIncomplete '<' = 4
scoreIncomplete _ = undefined

-- Solve Part 1.
solve1 :: [Parser] -> Integer
solve1 = sum . map scoreCorrupt . rights . map parseChunks

-- Solve Part 2.
solve2 :: [Parser] -> Integer
solve2 = median . map score . lefts . map parseChunks
    where score = foldl (\acc x -> 5 * acc + x) 0 . map scoreIncomplete
          median xs = sort xs !! (length xs `div` 2)

-- Print solutions.
main :: IO ()
main = do input >>= print . solve1
          input >>= print . solve2
