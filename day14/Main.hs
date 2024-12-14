module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day14 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let example_space = (11,7)
        actual_space = (101,103)
        space = if length parsedInput > 15 then actual_space else example_space
    let answer1 = (\(q1,q2,q3,q4) -> q1*q2*q3*q4)
                $ toQuadrants space $ walk space 100 parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1

    -- Visualise states to find the easter egg manually
    let states = map (\i -> (i,walk space i parsedInput)) [22,123..]
    -- (NOTE: We do 22,123, as starting at 22 and then every 101 steps I see
    --        something that has some structure to it, so I expected that to become
    --        the image and hence I loop only through them to speed up finding the result.
    -- (NOTE: this will go on infinitely, so when you spot the Christmas tree stop it)
    mapM_ (\(i,s) -> do putStrLn $ "-------------# " ++ show i
                        mapM_ putStrLn $ toCountGrid space s) states

type Vec = (Int,Int)
type Robot = (Vec,Vec)

parse :: String -> [Robot]
parse input = map (Common.mapTuple toVec . Common.splitOnceOn " ")
            $ lines input
                where toVec = Common.mapTuple read
                            . Common.splitOnceOn "," . drop 2


walk :: (Int,Int) -> Int -> [Robot] -> [Robot]
walk (sizeX,sizeY) steps = map (\((x,y),(vx,vy)) -> (((x+vx*steps) `mod` sizeX,
                                                      (y+vy*steps) `mod` sizeY),
                                                     (vx,vy)))

toQuadrants :: (Int,Int) -> [Robot] -> (Int,Int,Int,Int)
toQuadrants size robots = foldr foldOne (0,0,0,0) robots
    where (halfX,halfY) = Common.mapTuple (`div` 2) size
          foldOne ((x,y),_) qs@(q1,q2,q3,q4)
            | x > halfX && y > halfY = (q1,q2,q3,q4+1)
            | x > halfX && y < halfY = (q1,q2+1,q3,q4)
            | x < halfX && y > halfY = (q1,q2,q3+1,q4)
            | x < halfX && y < halfY = (q1+1,q2,q3,q4)
            | otherwise = qs

toCountGrid :: (Int,Int) -> [Robot] -> [String]
toCountGrid (sizeX,sizeY) robots =
    map (\y -> concatMap (\x -> f (x,y) positions) [0..sizeX-1]) [0..sizeY-1]
    where f p ps = case length $ filter (==p) ps of
                     0 -> " "
                     x -> show x
          positions = map fst robots
