module Main where

import qualified Common
import Data.List (sortOn,sort)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day24 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(state,gates) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let zeroPad n xs = (take (max 0 (n - length xs)) $ repeat 0) ++ xs
        resetWith x y = foldr (\(w,v) -> M.insert w v) state
                      $ (map (\(i,v) -> ('x':(zfill2 i), v)) $ zip [0..] $ reverse xBin)
                     ++ (map (\(i,v) -> ('y':(zfill2 i), v)) $ zip [0..] $ reverse yBin)
            where xBin = zeroPad 44 $ toBinary x
                  yBin = zeroPad 44 $ toBinary y
        run gs s = map snd . sortOn fst . M.toList $ M.filterWithKey (\k _ -> head k == 'z') $ simulate gs s
        swap a b gs = map (\(g,x) -> if x == a then (g,b) else (if x == b then (g,a) else (g,x))) gs
    let answer1 = toDecimal $ run gates state
    let answer2 = init . concatMap (++",") $ sort $ concatMap (\(a,b) -> [a,b]) swaps
            where test gs a b = (reverse $ run gs $ resetWith a b, zeroPad 45 $ toBinary (a+b))
                  testRes = test (foldr (\(a,b) g -> swap a b g) gates swaps) 17592186044416 27592186044416
                  -- went through the input manually and identified the following swapped gates,
                  -- and used the test function above to find the next bit where i had an error
                  swaps = [("vcf","z10"),("z17","fhg"),("fsq","dvb"),("z39","tnc")]

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type State = M.Map String Int
type Gate = ((String,String,String),String)

parse :: String -> (State,[Gate])
parse input = (foldr (\(_,w) -> M.insertWith old w 2) state gates', gates')
    where (wires,gates) = Common.splitOnceOn [""] $ lines input
          parseWire x = (a, read b) where (a,b) = Common.splitOnceOn ": " x
          parseGate x = ((a, gate, b), c)
              where (left,c) = Common.splitOnceOn " -> " x
                    [a,gate,b] = splitOn " " left

          gates' = map parseGate gates
          state = M.fromList $ map parseWire wires
          old _ b = b

simulate :: [Gate] -> State -> State
simulate gates s = go S.empty s
    where go seen state | length gates == S.size seen = state
          go seen state = go (S.insert ready seen) $ M.insert c res state
              where readyGates = filter (\(((wa,_,wb),_)) -> (state M.! wa) /= 2
                                                          && (state M.! wb) /= 2) gates
                    ready@((a,op,b),c) = head $ filter (not . (`S.member` seen)) readyGates
                    (a',b') = Common.mapTuple (state M.!) (a,b)
                    res | op == "AND" = if a' == b' && a' == 1 then 1 else 0
                        | op == "OR" = if a' == 1 || b' == 1 then 1 else 0
                        | op == "XOR" = if a' /= b' then 1 else 0
                        | otherwise = error "invalid binary op"

binaryAdd :: [Int] -> [Int] -> [Int]
binaryAdd a b
  | length a > length b = binaryAdd a $ (take (length a - length b) $ repeat 0) ++ b
  | length b > length a = binaryAdd b $ (take (length b - length a) $ repeat 0) ++ a
  | otherwise = reverse $ go 0 (reverse a) (reverse b)
  where go 1 [] [] = [1]
        go 0 [] [] = []
        go carry (a':as) (b':bs) = let r = carry + a' + b'
                                    in case r of
                                         3 -> 1:(go 1 as bs)
                                         2 -> 0:(go 1 as bs)
                                         x -> x:(go 0 as bs)
        go _ _ _ = error "invalid input to binaryAdd"

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = reverse (helper n)
  where
    helper 0 = []
    helper x = let (q, r) = x `divMod` 2 in r : helper q

zfill2 :: Int -> String
zfill2 x | x < 10 = '0':(show x)
         | otherwise = show x

toDecimal :: [Int] -> Int
toDecimal bin = go (0 :: Integer) bin
    where go _ [] = 0
          go i (1:xs) = (2^i) + go (i+1) xs
          go i (_:xs) = go (i+1) xs
