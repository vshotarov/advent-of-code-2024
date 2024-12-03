module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.Char (ord)

main :: IO ()
main = do
    putStrLn $ "-- Solving day03 --"
    input <- Common.readInput

    -- Solve
    let answer1 = getSumOfProducts . concat $ splitOn "don't()" input
    let answer2 = getSumOfProducts input

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

getSumOfProducts :: String -> Int
getSumOfProducts input = go [] [] False True input
    where go _ _ _ _ [] = 0
          go _ _ _ False ('d':'o':'(':')':xs) = go [] [] False True xs
          go _ _ _ True ('d':'o':'n':'\'':'t':'(':')':xs) = go [] [] False False xs
          go b [] True _ (',':xs) = go [] b True True xs
          go b a True _ (')':xs) = ((read a)*(read b)) + go [] [] False True xs
          go b a True _ (x:xs)
            | isDigit x = go (b ++ [x]) a True True xs
            | otherwise = go [] [] False True xs
          go [] [] False True ('m':'u':'l':'(':xs) = go [] [] True True xs
          go _ _ False d (x:xs) = go [] [] False d xs

isDigit :: Char -> Bool
isDigit x = ord x >= 48 && ord x <= 57
