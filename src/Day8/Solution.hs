{-# LANGUAGE BinaryLiterals #-}
module Main (main) where

import Control.Monad
import Data.Bits
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Word

-- DisplayMap holds [Display] representing unique numbers and output [Display].
data DisplayMap = DisplayMap [Display] [Display]
-- Display is Word8, with each bit representing a segment of the Display.
type Display = Word8

-- Puzzle input.
input :: IO [DisplayMap]
input = map parseInput . lines <$> readFile "input/Day8/input.txt"

-- Parse input.
parseInput :: String -> DisplayMap
parseInput s = DisplayMap uniqueDisplays outputDisplays
    where splitString = splitOn "|" s
          uniqueDisplays = map parseDisplay $ words $ splitString!!0
          outputDisplays = map parseDisplay $ words $ splitString!!1

-- Given String, return Display. Each Char represents a segment which is on.
parseDisplay :: String -> Display
parseDisplay s = sum $ zipWith (*) binList (map (2^) [0..])
    where binList = reverse $ map (\c -> if c `elem` s then 1 else 0) ['a'..'g']

-- [Display] representing all possible numbers which can be displayed.
validDisplays :: [Display]
validDisplays = [ 0b1110111
                , 0b0010010
                , 0b1011101
                , 0b1011011
                , 0b0111010
                , 0b1101011
                , 0b1101111
                , 0b1010010
                , 0b1111111
                , 0b1111011
                ]

-- Check if [Display] is valid when (Display -> Display) is applied.
validMapping :: (Display -> Display) -> [Display] -> Bool
validMapping f ds = partValidMapping f ds 7

-- Check if [Display] is valid for Int LSB when (Display -> Display) is applied.
partValidMapping :: (Display -> Display) -> [Display] -> Int -> Bool
partValidMapping f ds b = all (`elem` masked validDisplays) (masked $ map f ds)
    where mask = foldr ((.|.) . setBit 0) 0 [0 .. b - 1]
          masked = map (.&. mask)

-- Given Display, return the Int it represents.
decode :: Display -> Int
decode 0b1110111 = 0
decode 0b0010010 = 1
decode 0b1011101 = 2
decode 0b1011011 = 3
decode 0b0111010 = 4
decode 0b1101011 = 5
decode 0b1101111 = 6
decode 0b1010010 = 7
decode 0b1111111 = 8
decode 0b1111011 = 9
decode _ = undefined

-- Given two Int indiced and Display, return Display with indexed bits swapped.
swapBits :: Int -> Int -> Display -> Display
swapBits b1 b2 d = if b1 == b2 then d else swappedOne .|. swappedTwo
    where overwrite b1 b2 = if testBit d b1 then setBit d b2 else clearBit d b2
          swappedOne = clearBit (overwrite b1 b2) b1
          swappedTwo = clearBit (overwrite b2 b1) b2

-- Given [Display], find (Display -> Display) which makes all Display valid.
getValidMapping :: [Display] -> (Display -> Display)
getValidMapping ds = fromJust $ go id 0 ds
    where go f b ds
              | validMapping f ds = Just f
              | not $ partValidMapping f ds b = Nothing
              | otherwise = join
                          $ find isJust
                          $ map (\ob -> go (swapBits b ob . f) (b+1) ds) [b..7]

-- Solve Part 1.
solve1 :: [DisplayMap] -> Int
solve1 = length . filter uniqueSize . concatMap outputDisplays
    where outputDisplays (DisplayMap _ ds) = ds
          uniqueSize d = popCount d `elem` [2, 3, 4, 7]

-- Solve Part 2.
solve2 :: [DisplayMap] -> Int
solve2 = sum . map (displaysToInt . applyMap)
    where applyMap (DisplayMap ud od) = map (getValidMapping ud) od
          displaysToInt ds = sum $ zipWith (*) (digitList ds) (map (10^) [0..])
          digitList ds = reverse $ map decode ds

-- Print solutions.
main :: IO ()
main = do input >>= print . solve1
          input >>= print . solve2
