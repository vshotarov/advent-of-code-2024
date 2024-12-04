module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day04 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = lines input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let (numRows,numCols) = (length parsedInput, length $ head parsedInput)
    let answer1 = sum . map countXMAS
                -- diagonals
                $ map (\x -> walk parsedInput (1,1)          (x,0)) [0..numCols-1]
               ++ map (\y -> walk parsedInput (1,1)          (0,y)) [1..numRows-1]
               ++ map (\x -> walk parsedInput (-1,1)         (x,0)) [0..numCols-1]
               ++ map (\y -> walk parsedInput (-1,1) (numCols-1,y)) [1..numRows-1]
               -- rows
               ++ parsedInput
               -- columns
               ++ map (\x -> walk parsedInput (0,1) (x,0)) [0..numCols-1]
    let answer2 = countMAS parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

walk :: [String] -> (Int,Int) -> (Int,Int) -> String
walk s (dx,dy) (x,y)
  | x >= 0 && y >= 0 && x < (length $ head s) && y < length s =
      ((s !! y) !! x):(walk s (dx,dy) (x+dx,y+dy))
  | otherwise = []

countXMAS :: String -> Int
countXMAS [] = 0
countXMAS xs | length xs < 4 = 0
countXMAS ('X':'M':'A':'S':xs) = 1 + countXMAS ('M':'A':'S':xs)
countXMAS ('S':'A':'M':'X':xs) = 1 + countXMAS ('A':'M':'X':xs)
countXMAS (_:xs) = countXMAS xs

countMAS :: [String] -> Int
countMAS s = sum $ map check [(x,y) | x <- [1..maxX-1], y <- [1..maxY-1]]
    where (maxY,maxX) = (length s - 1, (length $ head s) - 1)
          getKernel (x,y) = map get [(x-1,y-1),(x+1,y-1),(x,y),(x-1,y+1),(x+1,y+1)]
                where get (xx,yy) = (s !! yy) !! xx
          check (x,y) = fromEnum $ getKernel (x,y) `elem` ["MMASS","SSAMM","MSAMS","SMASM"]
