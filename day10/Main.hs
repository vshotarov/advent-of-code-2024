module Main where

import qualified Common
import qualified Data.Map as M
import Data.List (nub)

main :: IO ()
main = do
    putStrLn $ "-- Solving day10 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let grid = M.fromList parsedInput
        trails = map (paths grid) [(h,e) | h <- trailheads, e <- trailends]
                where trailheads = map fst $ filter ((==0) . snd) parsedInput
                      trailends  = map fst $ filter ((==9) . snd) parsedInput
    let answer1 = length . nub . concat . filter (not . null)
                $ map (map (\t -> (head t, last t)) . filter (not . null)) trails
    let answer2 = sum $ map length trails

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Point = (Int,Int)

parse :: String -> [(Point,Int)]
parse input = [((x,y),read [c]) | (y,row) <- zip [0..] $ lines input,
                                  (x,c)   <- zip [0..] row]

paths :: M.Map Point Int -> (Point,Point) -> [[Point]]
paths grid (a,b) = go [[a]]
    where go [] = []
          go ([]:_) = error "empty path"
          go ((p@(x,y):path):queue)
            | p == b = (p:path):(go queue)
            | otherwise = let v = grid M.! p
                              ns = [(p':p:path)
                                   | p' <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)],
                                     M.findWithDefault (-1) p' grid == v+1]
                           in go (queue ++ ns)
