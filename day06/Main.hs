module Main where

import qualified Common
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day06 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(size,(obstacles,guard)) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)


    -- Solve
    let seen = S.map fst $ walk size guard obstacles
    let answer1 = S.size seen
    let answer2 = S.size . S.filter (S.null . walk size guard . (`S.insert` obstacles))
                $ S.filter (/=guard) seen

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Point = (Int,Int)
type Vec = (Int,Int)

parse :: String -> (Int,(S.Set Point,Point))
parse input = (length asLines, foldr foldLine (S.empty,(-1,-1)) $ zip [0..] asLines)
    where asLines = lines input
          foldLine (y,row) acc =
            foldr (\(x,c) (obs,g) -> (if c == '#' then S.insert (x,y) obs else obs,
                                      if c == '^' then (x,y) else g))
                  acc $ zip [0..] row

walk :: Int -> Point -> S.Set Point -> S.Set (Point,Vec)
walk size start obs = go S.empty (start,(0,-1))
    where isInBounds (x,y) = x >= 0 && y >= 0 && x < size && y < size
          go seen (p,d@(dx,dy))
            | (p,d) `S.member` seen = S.empty
            | otherwise = let path = takeWhile (\p' -> isInBounds p'
                                                    && not (p' `S.member` obs))
                                   $ iterate (\(x',y') -> (x'+dx,y'+dy)) p
                              seen' = S.union seen . S.fromList $ map (\p' -> (p',d)) path
                              p'@(x',y') = last (p:path)
                           in if (not $ isInBounds (x'+dx,y'+dy))
                                 then seen'
                                 else go seen' (p',(-dy,dx))
