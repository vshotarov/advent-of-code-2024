module Main where

import qualified Common
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)

main :: IO ()
main = do
    putStrLn $ "-- Solving day16 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(start,end,grid) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let (paths,costs) = dijkstra (start,(1,0)) $ toGraph grid
        endDirectionCosts = filter ((<maxCost) . snd)
                          $ map (\d -> (d, M.findWithDefault maxCost (end,d) costs))
                                [(1,0),(0,1),(-1,0),(0,-1)]
    let answer1 = minimum $ map snd endDirectionCosts
    let answer2 = sum $ map (\(d,_) -> 1 + (S.size . S.map fst $ paths M.! (end,d)))
                $ filter ((==answer1) . snd) endDirectionCosts

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec = (Int,Int)
type Edge = (Int,Vec,Vec)
type Graph = M.Map (Vec,Vec) (S.Set Edge)

maxCost :: Int
maxCost = 10000000000

parse :: String -> (Vec, Vec, [Vec])
parse input = (start,end,grid)
    where asLines = lines input
          (maxX,maxY) = ((length $ head asLines)-1, length asLines - 1)
          ps = [(x,y) | x <- [0..maxX], y <- [0..maxY]]
          grid = [(x,y) | (x,y) <- ps, (asLines !! y) !! x /= '#']
          (start,end) = Common.mapTuple (\c ->
                            Common.firstWhere (\(x,y) -> (asLines !! y) !! x == c) ps)
                            ('S','E')

toGraph :: [Vec] -> Graph
toGraph ps = M.fromList [((p,d), S.fromList $ ns (p,d))
                        | p <- ps, d <- [(1,0),(0,1),(-1,0),(0,-1)]]
    where ns ((x,y), (dx,dy)) = filter (\(_,p',_) -> p' `elem` ps)
                              $ [(   1, (x+dx,y+dy), ( dx, dy)),
                                 (1001, (x-dy,y+dx), (-dy, dx)),
                                 (1001, (x+dy,y-dx), ( dy,-dx))]

dijkstra :: (Vec,Vec) -> Graph -> (M.Map (Vec,Vec) (S.Set (Vec,Vec)), M.Map (Vec,Vec) Int)
dijkstra start graph = go M.empty (M.fromList [(start,0)]) (S.fromList [(0,start)])
    where go paths costs queue | S.null queue = (paths,costs)
          go paths costs queue = go paths' costs' queue''
              where getCost p = M.findWithDefault maxCost p costs
                    ((cost,u),queue') = fromJust $ S.minView queue
                    update (c,p,d) (ps,cs,q) = if alt <= getCost v
                                                  then (ps',cs',q')
                                                  else (ps,cs,q')
                        where v = (p,d)
                              alt = c + cost
                              ps' = M.insertWith S.union v (S.insert u $ M.findWithDefault S.empty u paths) ps
                              cs' = M.insert v alt cs
                              q' = if M.member v costs then q else S.insert (alt,v) q
                    (paths',costs',queue'') = foldr update (paths,costs,queue')
                                           $ graph M.! u
