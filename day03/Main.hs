module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.Char (ord)

main :: IO ()
main = do
    putStrLn $ "-- Solving day03 --"
    input <- Common.readInput

    -- Solve
    let answer1 = getProductSum input
    let answer2 = getProductSum first
                + ((sum . map (sum . map getProductSum . tail . splitOn "do()"))
                  $ splitOn "don't()" remaining)
            where (first,remaining) = Common.splitOnceOn "don't()" input

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

getProductSum :: String -> Int
getProductSum input = sum $ map (product . map read)
                    . filter (\x -> length x == 2 && all (\y -> (not $ null y) && all isDigit y) x)
                    . concatMap (map (splitOn ",") . splitOn ")") $ splitOn "mul(" input

isDigit :: Char -> Bool
isDigit x | ord x >= 48 && ord x <= 57 = True
isDigit _ = False
