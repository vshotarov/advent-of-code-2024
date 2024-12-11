module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.List (sortOn)

main :: IO ()
main = do
    putStrLn $ "-- Solving day11 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let solve n = sum . map snd . last . take (n+1)
                $ iterate (compact . sortOn fst . evolve) parsedInput
    let answer1 = solve 25
    let answer2 = solve 75

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Stone = (Int,Int)

parse :: String -> [Stone]
parse input = map (\s -> (read s,1)) $ splitOn " " input

evolve :: [Stone] -> [Stone]
evolve [] = []
evolve ((0,i):xs) = (1,i):(evolve xs)
evolve ((x,i):xs) = if numDigits `mod` 2 == 0 
                       then (read $ take (numDigits `div` 2) $ asString,i)
                           :(read $ drop (numDigits `div` 2) $ asString,i)
                           :(evolve xs)
                       else ((x*2024,i)):(evolve xs)
    where asString = show x
          numDigits = length asString

compact :: [Stone] -> [Stone]
compact [] = []
compact [x] = [x]
compact (a@(ai,ac):b@(bi,bc):xs)
  | ai == bi = compact ((ai,ac+bc):xs)
  | otherwise = a:(compact (b:xs))
