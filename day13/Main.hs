module Main where

import qualified Common
import Data.List.Split (splitOn)

main :: IO ()
main = do
    putStrLn $ "-- Solving day13 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = sum $ map cheapestWin parsedInput
    let answer2 = sum $ map cheapestWin machines
            where machines = map (\(a,b,p) -> (a,b,Common.mapTuple (+10000000000000) p))
                                 parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec = (Int,Int)
type Machine = (Vec,Vec,Vec)

parse :: String -> [Machine]
parse input = map (parseOne . lines) $ splitOn "\n\n" input
    where parseLine n = Common.mapTuple (read . dropWhile (=='+') . drop n)
                      . Common.splitOnceOn ", " . snd . Common.splitOnceOn ": "
          parseOne [a,b,prize] = (parseLine 1 a, parseLine 1 b,
                                  parseLine 2 prize)
          parseOne _ = error "malformatted input"

cheapestWin :: Machine -> Int
cheapestWin ((ax,ay),(bx,by),(px,py)) =
    if (a*ax+b*bx == px) && (a*ay+b*by == py) then a*3+b else 0
    where b = (ax*py - ay*px) `div` (ax*by - ay*bx)
          a = (px - bx * b) `div` ax
