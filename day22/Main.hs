module Main where

import qualified Common
import Data.Bits (xor)
import Data.Char (ord)
import qualified Data.Map as M
import Data.List (sortOn)

main :: IO ()
main = do
    putStrLn $ "-- Solving day22 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = sum $ map (last . take 2001 . iterate step) parsedInput
    let answer2 = snd . last $ sortOn snd $ M.toList
                $ foldr1 (\m acc -> foldr (\(p,v) -> M.insertWith (+) p v) acc $ M.toList m)
                $ map (\x -> getPatterns $ map ones $ take 2005 $ iterate step x) parsedInput 


    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [Int]
parse input = map read $ lines input

step :: Int -> Int
step x = x'''
    where mix a b = xor a b
          prune = (`mod` 16777216)
          x' = prune . mix x $ x * 64
          x'' = prune . mix x' $ x' `div` 32
          x''' = prune . mix x'' $ x'' * 2048

ones :: Int -> Int
ones x = (ord . last $ show x) - 48

getPatterns :: [Int] -> M.Map [Int] Int
getPatterns ns = go M.empty [] ns
    where toChanges xs = map (\(a,b) -> b-a) $ zip xs $ tail xs
          go patterns _ [] = patterns
          go patterns buffer (x:xs)
            | length buffer < 5 = go patterns (buffer ++ [x]) xs
            | M.member (toChanges buffer) patterns = go patterns (tail $ buffer ++ [x]) xs
            | otherwise = go (M.insert (toChanges buffer) (last buffer) patterns) (tail $ buffer ++ [x]) xs
