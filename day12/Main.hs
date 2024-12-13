module Main where

import qualified Common
import qualified Data.Set as S
import Data.List (nub)

main :: IO ()
main = do
    putStrLn $ "-- Solving day12 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = lines input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let regions = getRegions parsedInput
        solve fenceFunc = sum $ map (\(s,_) -> (S.size s) * (fenceFunc s)) regions
    let answer1 = solve perimeter
    let answer2 = solve sides

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Point = (Int,Int)

neighbours :: Point -> [Point]
neighbours (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

perimeter :: S.Set Point -> Int
perimeter ps = sum . map ((4-) . length . filter (`S.member` ps) . neighbours) $ S.toList ps

getRegions :: [String] -> [(S.Set Point,Char)]
getRegions grid = go S.empty [(x,y) | x <- [0..maxX], y <- [0..maxY]]
    where (maxX,maxY) = ((length $ head grid)-1, (length grid)-1)
          isValid (x,y) = x >= 0 && x <= maxX && y >= 0 && y <= maxY
          get (x,y) = (grid !! y) !! x
          bfs _ seen [] = seen
          bfs c seen (p:queue)
            | (not $ isValid p) || (get p) /= c || p `S.member` seen
                        = bfs c seen queue
            | otherwise = bfs c (p `S.insert` seen) (queue ++ neighbours p)
          go _ [] = []
          go seen (p:ps)
            | p `S.member` seen = go seen ps
            | otherwise = let c = get p 
                              region = bfs c S.empty [p]
                           in (region,c):(go (S.union seen region) ps)

sides :: S.Set Point -> Int
sides cells = sum $ (map sidesOnRow [minY-1..maxY+1]
                  ++ map sidesOnCol [minX-1..maxX+1])
    where (xs,ys) = unzip $ S.toList cells
          (minX,minY) = (minimum xs, minimum ys)
          (maxX,maxY) = (maximum xs, maximum ys)
          sidesOnRowOrCol i f r = sum . map (fromEnum . hasFence) $ zip aa $ tail aa
              where aa = []:(map (\j -> map (`S.member` cells) [f i j, f (i-1) j]) r)
                    hasFence (a,b) = a /= b && length (nub b) == 2
          sidesOnRow y = sidesOnRowOrCol y (\i j -> (j,i)) [minX..maxX]
          sidesOnCol x = sidesOnRowOrCol x (\i j -> (i,j)) [minY..maxY]

