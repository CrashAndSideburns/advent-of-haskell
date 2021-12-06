module Main (main) where

import Data.List
import Data.List.Split
import qualified Data.Map as M

-- FishBins is a Map mapping fish internal timers to populations.
type FishBins = M.Map Int Int

-- Puzzle input.
input :: IO FishBins
input = parseInput <$> readFile "input/Day6/input.txt"

-- Given a String of fish internal timers, return corresponding FishBins.
parseInput :: String -> FishBins
parseInput = M.fromList . map makeBin . group . sort . map read . splitOn ","
    where makeBin ns = (head ns, length ns)

-- Given FishBins, decrement the internal timers of all fish by 1.
ageFish :: FishBins -> FishBins
ageFish = M.mapKeys (subtract 1)

-- Given FishBins, add appropriate new fish with internal timer 8.
addFish :: FishBins -> FishBins
addFish fs = M.insert 8 (M.findWithDefault 0 (-1) fs) fs

-- Given FishBins, restore all internal timers at -1 to 6.
renewFish :: FishBins -> FishBins
renewFish fs = M.delete (-1)
             $ M.insertWith (+) 6 (M.findWithDefault 0 (-1) fs) fs

-- Given FishBins, take one step in the simulation: age, add, and renew fish.
step :: FishBins -> FishBins
step = renewFish . addFish . ageFish

-- Solve Part 1.
solve1 :: FishBins -> Int
solve1 fs = M.foldr (+) 0 $ iterate step fs !! 80

-- Solve Part 2.
solve2 :: FishBins -> Int
solve2 fs = M.foldr (+) 0 $ iterate step fs !! 256

-- Print solutions.
main :: IO ()
main = do input >>= print . solve1
          input >>= print . solve2
