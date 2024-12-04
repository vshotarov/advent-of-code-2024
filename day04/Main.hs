module Main where

import qualified Common
import Data.List (transpose)

main :: IO ()
main = do
    putStrLn $ "-- Solving day04 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = lines input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = sum . map countXMAS
                $ diagonals parsedInput
               ++ parsedInput
               ++ transpose parsedInput
    let answer2 = countMAS parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

countXMAS :: String -> Int
countXMAS [] = 0
countXMAS xs | length xs < 4 = 0
countXMAS ('X':'M':'A':'S':xs) = 1 + countXMAS ('M':'A':'S':xs)
countXMAS ('S':'A':'M':'X':xs) = 1 + countXMAS ('A':'M':'X':xs)
countXMAS (_:xs) = countXMAS xs

countMAS :: [String] -> Int
countMAS s = go [(x,y) | x <- [1..numCols-1], y <- [1..numRows-2]]
    where (numRows,numCols) = (length s, length $ head s)
          get (x,y) = (s !! y) !! x
          check (x,y) = let d = map get [(x-1,y-1),(x+1,y-1),(x,y),(x-1,y+1),(x+1,y+1)]
                         in d `elem` ["MMASS","SSAMM","MSAMS","SMASM"]
          go [] = 0
          go ((x,y):xs)
            | x < numCols-1 && y < numRows-1 = (fromEnum $ check (x,y)) + go xs
            | otherwise = go xs

diagonals :: [String] -> [String]
diagonals xs = map (\x -> pos (x,0)) [0..numCols-1]
            ++ map (\y -> pos (0,y)) [1..numRows-1]
            ++ map (\x -> neg (x,0)) [0..numCols-1]
            ++ map (\y -> neg (numCols-1,y)) [1..numRows-1]
    where (numRows,numCols) = (length xs, length $ head xs)
          pos (x,y)
                | x < numCols && y < numRows = ((xs !! y) !! x):(pos (x+1,y+1))
                | otherwise = []
          neg (x,y)
                | x >= 0 && y < numRows = ((xs !! y) !! x):(neg (x-1,y+1))
                | otherwise = []

