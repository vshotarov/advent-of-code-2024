module Main where

import qualified Common
import Data.List.Split (splitOn)

main :: IO ()
main = do
    putStrLn $ "-- Solving day07 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let concat' a b = read (show a ++ show b)
        solve ops = sum . map fst $ filter (uncurry $ test ops) parsedInput
    let answer1 = solve [(+),(*)]
    let answer2 = solve [(+),(*),concat']

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [(Integer,[Integer])]
parse input = map parseOne $ lines input
    where parseOne x = (read left, map read $ splitOn " " right)
                where (left,right) = Common.splitOnceOn ": " x

test :: [(Integer -> Integer -> Integer)] -> Integer -> [Integer] -> Bool
test _ _ [] = error "empty list of operands"
test ops left (n:ns) = go [n] ns
    where go results [] = any (==left) results
          go results (x:xs) = go (concatMap (applyOps x) results) xs
          applyOps b a = filter (<=left) $ map (\op -> op a b) ops
