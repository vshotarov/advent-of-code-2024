module Main where

import qualified Common
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)

main :: IO ()
main = do
    putStrLn $ "-- Solving day20 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(start,end,graph) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let distsFromEnd = dijkstra end graph
        distsFromStart = dijkstra start graph
        solve range =
            length . filter (>=100) . map ((distsFromStart M.! end)-)
          . filter (\c -> c/=(-1) && c < distsFromStart M.! end)
          . concat $ foldr (\p acc ->
              (map (\p' -> if M.member p' distsFromEnd
                              then (distsFromStart M.! p)
                                 + (manhattan p p')
                                 + (distsFromEnd M.! p')
                              else -1) $ ns p):acc) [] $ M.keys graph
            where manhattan (x,y) (x',y') = abs (x'-x) + abs (y'-y)
                  ns (x,y) = [(x',y') | x' <- [x-range..x+range], y' <- [y-range..y+range],
                                        manhattan (x,y) (x',y') <= range]
    let answer1 = solve 2
    let answer2 = solve 20

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec = (Int,Int)
type Edge = (Int,Vec)
type Graph = M.Map Vec (S.Set Edge)

maxCost :: Int
maxCost = 10000000000

parse :: String -> (Vec, Vec, Graph)
parse input = (start,end,toGraph grid)
    where asLines = lines input
          (maxX,maxY) = ((length $ head asLines)-1, length asLines - 1)
          ps = [(x,y) | x <- [0..maxX], y <- [0..maxY]]
          grid = [(x,y) | (x,y) <- ps, (asLines !! y) !! x /= '#']
          (start,end) = Common.mapTuple (\c ->
                            Common.firstWhere (\(x,y) -> (asLines !! y) !! x == c) ps)
                            ('S','E')

toGraph :: [Vec] -> Graph
toGraph ps = M.fromList [(p,S.fromList $ map (\n -> (1,n)) $ ns)
                        | p@(x,y) <- ps,
                          let ns = filter (`elem` ps)
                                   [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]]

dijkstra :: Vec -> Graph -> M.Map Vec Int
dijkstra start graph = go (M.fromList [(start,0)]) (S.fromList [(0,start)])
    where go costs queue | S.null queue = costs
          go costs queue = go costs' queue''
              where getCost p = M.findWithDefault maxCost p costs
                    ((cost,u),queue') = fromJust $ S.minView queue
                    update (c,p) (cs,q) = if alt <= getCost p
                                                then (cs',q')
                                                else (cs,q')
                        where alt = c + cost
                              cs' = M.insert p alt cs
                              q' = if M.member p costs then q else S.insert (alt,p) q
                    (costs',queue'') = foldr update (costs,queue')
                                           $ graph M.! u
