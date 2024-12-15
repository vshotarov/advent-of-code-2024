module Main where

import qualified Common
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day15 --"
    input <- Common.readInput

    -- Parse
    let parsedInput1 = parse input
        parsedInput2 = parse $ expandGrid input
    putStrLn $ "Parsed input1: " ++ (Common.truncateString $ show parsedInput1)
    putStrLn $ "Parsed input2: " ++ (Common.truncateString $ show parsedInput2)

    -- Solve
    let toGpsSum boxChar = sum . map ((\(x,y) -> y*100 + x) . fst)
                                     . filter ((==boxChar) . snd)
                                     . M.toList . (\(_,g,_) -> g)
    let answer1 = toGpsSum 'O' $ walk1 parsedInput1
    let answer2 = toGpsSum '[' $ walk2 parsedInput2

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec = (Int,Int)
type Point = Vec
type State = (Point,M.Map Vec Char,[Vec])

parse :: String -> State
parse input = (robot, M.insert robot '.' grid', map toDir $ concat moves)
    where (grid,moves) = Common.mapTuple lines $ Common.splitOnceOn "\n\n" input
          grid' = M.fromList [((x,y),c) | x <- [0..(length $ head grid)-1],
                                          y <- [0..(length grid)-1],
                                          let c = (grid !! y) !! x]
          robot = fst . Common.firstWhere ((=='@') . snd) $ M.toList grid'
          toDir '^' = (0,-1)
          toDir '<' = (-1,0)
          toDir '>' = (1,0)
          toDir 'v' = (0,1)
          toDir x = error ("uncrecognised move instruction " ++ [x])

expandGrid :: String -> String
expandGrid [] = []
expandGrid ('@':xs) = '@':'.':(expandGrid xs)
expandGrid ('O':xs) = '[':']':(expandGrid xs)
expandGrid (x:xs)
  | x `elem` "\nv^<>" = x:(expandGrid xs)
  | otherwise = x:x:(expandGrid xs)


vecAdd :: Vec -> Vec -> Vec
vecAdd (ax,ay) (bx,by) = (ax+bx,ay+by)

walk1 :: State -> State
walk1 s@(_,_,[]) = s
walk1 (p@(x,y),grid,(d@(dx,dy):ds))
  | M.findWithDefault '#' (x+dx,y+dy) grid == '#' = walk1 (p,grid,ds)
  | M.findWithDefault '#' (x+dx,y+dy) grid == '.' = walk1 ((x+dx,y+dy),grid,ds)
  | otherwise = let boxes = takeWhile (\b -> grid M.! b == 'O') $ iterate (vecAdd d) (x+dx,y+dy)
                    next = vecAdd (last boxes) d
                    (p',grid') = if get next == '#' then (p,grid) else ((x+dx,y+dy),(M.insert next 'O' $ M.insert p '.' grid))
                 in walk1 (p',M.insert p' '.' grid',ds)
  where get (x',y') = M.findWithDefault '#' (x',y') grid

walk2 :: State -> State
walk2 s@(_,_,[]) = s
walk2 (p@(x,y),grid,(d@(dx,dy):ds))
  | M.findWithDefault '#' (x+dx,y+dy) grid == '#' = walk2 (p,grid,ds)
  | M.findWithDefault '#' (x+dx,y+dy) grid == '.' = walk2 ((x+dx,y+dy),grid,ds)
  | otherwise = let getBoxes seen [] = S.toList seen
                    getBoxes seen ((x',y'):xs)
                      | (x',y') `S.member` seen = getBoxes seen xs
                      | get (x',y') == '[' = getBoxes (S.insert (x',y') seen) (xs ++ [(x'+dx,y'+dy),(x'+1,y')])
                      | get (x',y') == ']' = getBoxes (S.insert (x',y') seen) (xs ++ [(x'+dx,y'+dy),(x'-1,y')])
                      | otherwise = getBoxes seen xs
                    boxes = getBoxes S.empty [vecAdd p d]
                    nexts = map (vecAdd d) boxes
                    clearedGrid = foldr (\b -> M.insert b '.') grid boxes
                    (p',grid') = if any ((=='#') . get) nexts then (p,grid) else ((x+dx,y+dy),
                        foldl (\acc (x',y') -> M.insert (x'+dx,y'+dy) (grid M.! (x',y')) acc) clearedGrid boxes)
                 in walk2 (p',M.insert p' '.' grid',ds)
  where get (x',y') = M.findWithDefault '#' (x',y') grid

display :: M.Map Vec Char -> String
display xs = let maxX = maximum $ map fst $ M.keys xs
                 maxY = maximum $ map snd $ M.keys xs
              in concatMap (\y -> (map (\x -> xs M.! (x,y)) [0..maxX]) ++ "\n") [0..maxY]
