module Main where

import qualified Common
import Data.List.Split (splitOn)
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day19 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(towels,designs) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let arrangements = map (memo M.!) designs
            where memo = foldr (fit towels) M.empty designs
    let answer1 = length $ filter (/=0) arrangements
    let answer2 = sum arrangements

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Cache = M.Map String Int

parse :: String -> ([String],[String])
parse input = (splitOn ", " towels, lines designs)
    where (towels,designs) = Common.splitOnceOn "\n\n" input

fit :: [String] -> String -> Cache -> Cache
fit towels design memo
  | M.member design memo = memo
  | otherwise = (uncurry $ M.insert design) $ foldr foldf (0,memo) towels
  where foldf towel (n,m)
          | towel == design = (n+1,m)
          | towel == take (length towel) design = (n + m' M.! design', m')
              where design' = drop (length towel) design
                    m' = fit towels design' m
        foldf _ state = state
