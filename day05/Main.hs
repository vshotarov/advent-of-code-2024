module Main where

import qualified Common
import Data.List.Split (splitOn)
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day05 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(rules,updates) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let solve cond = sum $ map (\u -> f' u $ reorder rules u) updates
            where f' update reordered
                    | cond update reordered = reordered !! ((length reordered) `div` 2)
                    | otherwise = 0
    let answer1 = solve (==)
    let answer2 = solve (/=)

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> ([(Int,Int)],[[Int]])
parse input = (map (Common.mapTuple read . Common.splitOnceOn "|") rules
              ,map (map read . splitOn ",") updates)
    where (rules,updates) = Common.mapTuple lines $ Common.splitOnceOn "\n\n" input

reorder :: [(Int,Int)] -> [Int] -> [Int]
reorder rules xs = if null offenders then xs else reorder rules xs'
    where order = M.fromList $ zip xs [0..] :: M.Map Int Int
          offenders = filter (\(a,b) -> M.member a order
                                     && M.member b order
                                     && (order M.! a) > (order M.! b)) rules
          xs' = map (\x -> if x == oa then ob else (if x == ob then oa else x)) xs
              where (oa,ob) = head offenders
