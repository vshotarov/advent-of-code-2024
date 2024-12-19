module Main where

import qualified Common
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day18 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let (maxX,maxY,p1Steps) = if length parsedInput > 100
                                 then (70,70,1024)
                                 else (6,6,12)
        states = getAllStates (maxX,maxY) parsedInput
        path1 = path (states !! p1Steps) (0,0) (maxX,maxY)

    let answer1 = length $ path (states !! p1Steps) (0,0) (maxX,maxY)
    let answer2 = go path1 . drop (p1Steps+1) $ zip parsedInput states
            where go _ [] = error "no bytes block the path"
                  go best ((byte,state):states')
                    | byte `elem` best = let best' = path state (0,0) (maxX,maxY)
                                          in if best' == [(-1,-1)]
                                                then byte
                                                else go best' states'
                    | otherwise = go best states'

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec = (Int,Int)
type Graph = M.Map Vec [Vec]

parse :: String -> [(Int,Int)]
parse input = map (Common.mapTuple read . Common.splitOnceOn ",") $ lines input

path :: Graph -> Vec -> Vec -> [Vec]
path graph start end = go S.empty [[start]]
    where go _ [] = [(-1,-1)]
          go seen ((n:p):queue)
            | n == end = p
            | n `elem` seen = go seen queue
            | otherwise = go (S.insert n seen)
                        $ queue ++ (map (\n' -> n':n:p) $ graph M.! n)


getAllStates :: (Int,Int) -> [Vec] -> [Graph]
getAllStates (maxX,maxY) bytes = tail $ scanl updateGraph graph bytes
    where graph = M.fromList
                $ [((x,y),ns) | x <- [0..maxX], y <- [0..maxY],
                                let ns = filter (`elem` positions)
                                       $ [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]]
                  where positions = [(x,y) | x <- [0..maxX], y <- [0..maxY]]
          updateGraph g p = M.map (filter (/=p))
                          $ M.filterWithKey (\k _ -> not (k == p)) g
