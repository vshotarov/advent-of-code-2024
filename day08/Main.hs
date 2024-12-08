module Main where

import qualified Common
import Data.List (nub,transpose)

main :: IO ()
main = do
    putStrLn $ "-- Solving day08 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@((maxX,maxY),antennae) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    let inBounds (x,y) = x >= 0 && y >= 0 && x <= maxX && y <= maxY
        interlace a b = concat $ transpose [a,b]
        antinodes a@(ax,ay) b@(bx,by) =
            ((ax-dx,ay-dy):(bx+dx,by+dy):a:b:(interlace (walk 3) (walk (-2))))
             where (dx,dy) = (bx-ax,by-ay)
                   walk i = if inBounds p
                               then p:(walk $ if i >= 0 then (i+1) else (i-1))
                               else []
                       where p = (ax+i*dx,ay+i*dy)

    -- Solve
    let pairs = [(a,b) | (a,ac) <- antennae,
                         (b,bc) <- antennae,
                         a /= b && ac == bc]
    let answer1 = length . nub . filter inBounds $ concatMap (take 2 . uncurry antinodes) pairs
    let answer2 = length . nub . filter inBounds $ concatMap (uncurry antinodes) pairs

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Point = (Int,Int)

parse :: String -> ((Int,Int),[(Point,Char)])
parse input = (((length $ head asLines) -1, length asLines -1),
               [((x,y),a) | (y,row) <- zip [0..] asLines,
                           (x,a)   <- zip [0..] row,
                           a /= '.'])
    where asLines = lines input
