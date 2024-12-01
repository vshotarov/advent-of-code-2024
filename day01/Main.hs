module Main where

import qualified Common
import Data.List (sort)

main :: IO ()
main = do
    putStrLn $ "-- Solving day01 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(left,right) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = sum . map (\(a,b) -> abs (b - a)) $ zip (sort left) (sort right)
    let answer2 = sum $ map (\l -> l * count right l) left

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> ([Int],[Int])
parse input = unzip
            . map ((\(a,b) -> (read a, read b)) . (Common.splitOnceOn "   "))
            $ lines input

count :: Eq a => [a] -> a -> Int
count [] _ = 0
count (x:xs) a
  | x == a    = 1 + count xs a
  | otherwise = count xs a
