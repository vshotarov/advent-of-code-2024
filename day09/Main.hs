module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day09 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = hash $ compact parsedInput
    let answer2 = hash $ compact2 parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

data Memory = Block Int Int
            | Space Int
            deriving (Show,Eq)

isSpace :: Memory -> Bool
isSpace (Space _) = True
isSpace _ = False

size :: Memory -> Int
size (Space s) = s
size (Block _ s) = s

parse :: String -> [Memory]
parse input = reverse $ go 0 [] input
    where go _ mem [] = mem
          go i mem@((Block _ _):_) (x:xs) = go i ((Space (read [x])):mem) xs
          go i mem (x:xs) = go (i+1) ((Block i (read [x])):mem) xs

compact :: [Memory] -> [Memory]
compact [] = []
compact (b@(Block _ _):mem) = b:(compact mem)
compact mem | isSpace (last mem) = compact $ init mem
compact ((Space s):mem) = let b@(Block i bs) = last mem
                           in case compare s bs of
                                EQ -> b:(compact $ init mem)
                                GT -> b:(compact ((Space (s-bs)):(init mem)))
                                LT -> (Block i s):(compact $ (init mem) ++ [Block i (bs-s)])

hash :: [Memory] -> Int
hash memory = go 0 0 memory
    where go _ h [] = h
          go c h ((Block i bs):mem) = go (c+bs) h' mem
              where h' = h + (sum $ map (*i) [c..(c+bs)-1])
          go c h ((Space s):mem) = go (c+s) h mem

compact2 :: [Memory] -> [Memory]
compact2 memory = let maxI = maximum $ map (\(Block i _) -> i) $ filter (not . isSpace) memory
                 in foldr foldF memory [0..maxI]
    where move _ [] = []
          move (Block i _) mem@((Block i' _):_) | i' == i = mem
          move b@(Block _ bs) ((Space s):mem)
            | s > bs  = b:(Space (s-bs)):(map (\b' -> if b'/=b then b' else (Space bs)) mem)
            | s == bs = b:(map (\b' -> if b'/=b then b' else (Space bs)) mem)
          move b (m:mem) = m:(move b mem)
          compactSpaces [] = []
          compactSpaces [x] = [x]
          compactSpaces ((Space a):(Space b):mem) = compactSpaces ((Space (a+b)):mem)
          compactSpaces (b':b'':mem) = b':(compactSpaces (b'':mem))
          foldF i mem = compactSpaces
                      $ move (head $ filter (\(Block bi _) -> i == bi)
                      $ filter (not . isSpace) mem) mem
