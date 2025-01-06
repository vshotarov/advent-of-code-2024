module Main where

import qualified Common
import Data.List (transpose)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    putStrLn $ "-- Solving day25 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(keys,locks) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length [(k,l)
                         | k <- keys, l <- locks,
                           not . any (>5) $ map (uncurry (+)) $ zip k l]
    let answer2 = "found him!"

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> ([[Int]],[[Int]])
parse input = foldr parseOne ([],[]) . splitOn [""] $ lines input
    where columns = map ((\x -> x-1) . length . filter (=='#')) . transpose
          parseOne one (keys,locks)
            | all (=='#') $ head one = ((columns one):keys,locks)
            | otherwise = (keys,(columns one):locks)
