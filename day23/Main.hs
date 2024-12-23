module Main where

import qualified Common
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (sort,sortOn,nub)

main :: IO ()
main = do
    putStrLn $ "-- Solving day23 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let graph = foldr (\(a,b) -> M.insertWith S.union b (S.singleton a)
                               . M.insertWith S.union a (S.singleton b))
                      M.empty parsedInput
    let answer1 = length . nub . concatMap (map sort . setsOf3StartingAt graph)
                . filter ((=='t') . head) $ M.keys graph
    let answer2 = init . concatMap (++",") . sort . S.toList . last
                . sortOn S.size $ map (findLargestSetsStartingAt graph) $ M.keys graph

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Graph = M.Map String (S.Set String)

parse :: String -> [(String,String)]
parse input = map (Common.splitOnceOn "-") $ lines input

setsOf3StartingAt :: Graph -> String -> [[String]]
setsOf3StartingAt graph start =
    nub . sort . concatMap (\(b,cs) -> map (\c -> sort [start,b,c]) cs)
  $ S.toList . S.filter (not . null . snd)
  $ S.map (\(x,d) -> (x,filter ((start `S.member`) . (graph M.!)) $ S.toList d))
  $ S.map (\x -> (x,graph M.! x)) (graph M.! start)


findLargestSetsStartingAt :: Graph -> String -> S.Set String
findLargestSetsStartingAt graph start = foldr foldF (S.singleton start)
                                      $ S.toList (graph M.! start)
    where foldF x acc
            | all ((S.member x) . (graph M.!)) $ S.toList acc = S.insert x acc
            | otherwise = acc
