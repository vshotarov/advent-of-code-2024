module Main where

import qualified Common
import Data.List.Split (splitOn)

main :: IO ()
main = do
    putStrLn $ "-- Solving day02 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length $ filter isValid1 parsedInput
    let answer2 = length $ filter (any isValid1 . toSkipped) parsedInput
            where toSkipped xs = map (\i -> take i xs ++ drop (i+1) xs) [0..length xs-1]

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [[Int]]
parse input = map (map read . splitOn " ") $ lines input

isValid1 :: [Int] -> Bool
isValid1 []  = True
isValid1 [_] = True
isValid1 (x:y:xs) = go (x:y:xs)
    where d = if y > x then 1 else (-1) :: Int
          go [] = True
          go [_] = True
          go (a:b:as)
            | d == 1 && (b-a) > 0 && (b-a) < 4 = go (b:as)
            | d == (-1) && (a-b) > 0 && (a-b) < 4 = go (b:as)
            | otherwise = False
