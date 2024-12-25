module Main where

import qualified Common
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Bits (xor)

main :: IO ()
main = do
    putStrLn $ "-- Solving day17 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(registers,operations) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = init . concatMap ((++",") . show) $ process registers operations 0
    let answer2 = firstWhereLast16DigitsMatch
            where process' a = process (M.insert 'A' a registers) operations 0
                  -- I am leaving this horribly verbose solution as a reminder for
                  -- the excrutiating process I went through to figure it out
                  equalToN n xs = (take n $ reverse operations) == (take n $ reverse xs)
                  firstWhereNDigitsMatch n start step = head
                                                      $ filter (equalToN n . process') [start,start+step..]
                  firstWhereLast1DigitsMatch = firstWhereNDigitsMatch 1 (8^15) ((8^14) :: Int)
                  firstWhereLast2DigitsMatch = firstWhereNDigitsMatch 2 firstWhereLast1DigitsMatch (8^13)
                  firstWhereLast3DigitsMatch = firstWhereNDigitsMatch 3 firstWhereLast2DigitsMatch (8^12)
                  firstWhereLast4DigitsMatch = firstWhereNDigitsMatch 4 firstWhereLast3DigitsMatch (8^11)
                  firstWhereLast5DigitsMatch = firstWhereNDigitsMatch 5 firstWhereLast4DigitsMatch (8^10)
                  firstWhereLast6DigitsMatch = firstWhereNDigitsMatch 6 firstWhereLast5DigitsMatch (8^9)
                  firstWhereLast7DigitsMatch = firstWhereNDigitsMatch 7 firstWhereLast6DigitsMatch (8^8)
                  firstWhereLast8DigitsMatch = firstWhereNDigitsMatch 8 firstWhereLast7DigitsMatch (8^7)
                  firstWhereLast9DigitsMatch = firstWhereNDigitsMatch 9 firstWhereLast8DigitsMatch (8^6)
                  firstWhereLast10DigitsMatch = firstWhereNDigitsMatch 10 firstWhereLast9DigitsMatch (8^5)
                  firstWhereLast11DigitsMatch = firstWhereNDigitsMatch 11 firstWhereLast10DigitsMatch (8^4)
                  firstWhereLast12DigitsMatch = firstWhereNDigitsMatch 12 firstWhereLast11DigitsMatch (8^3)
                  firstWhereLast13DigitsMatch = firstWhereNDigitsMatch 13 firstWhereLast12DigitsMatch (8^2)
                  firstWhereLast14DigitsMatch = firstWhereNDigitsMatch 14 firstWhereLast13DigitsMatch (8^1)
                  firstWhereLast15DigitsMatch = firstWhereNDigitsMatch 15 firstWhereLast14DigitsMatch (8^0)
                  firstWhereLast16DigitsMatch = firstWhereNDigitsMatch 16 firstWhereLast15DigitsMatch (8^0)

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Registers = M.Map Char Int

parse :: String -> (Registers, [Int])
parse input = (registers', map read . splitOn "," $ drop 9 $ head ops)
    where (registers,ops) = Common.splitOnceOn [""] $ lines input
          registers' = foldr (\reg -> M.insert (reg !! 9) $ regValue reg)
                             M.empty registers
              where regValue = read . snd . Common.splitOnceOn ": "

combo :: Registers -> Int -> Int
combo reg x
  | x < 4 = x
  | x == 7 = error "7"
  | x == 4 = reg M.! 'A'
  | x == 5 = reg M.! 'B'
  | x == 6 = reg M.! 'C'
  | otherwise = error "unrecognised operand"

process :: Registers -> [Int] -> Int -> [Int]
process _ ops pointer | pointer < 0 || pointer >= length ops = []
process _ _ pointer | odd pointer = error "odd pointer"
process reg ops pointer =
    case (ops !! pointer, ops !! (pointer+1)) of
      (0,operand) -> process (M.insert 'A' res reg) ops (pointer + 2)
                     where res = (reg M.! 'A') `div` (2 ^ (combo reg operand))
      (1,operand) -> process (M.insert 'B' res reg) ops (pointer + 2)
                     where res = xor (reg M.! 'B') operand
      (2,operand) -> process (M.insert 'B' res reg) ops (pointer + 2)
                     where res = (combo reg operand) `mod` 8
      (3,operand) -> if reg M.! 'A' == 0
                        then process reg ops (pointer + 2)
                        else process reg ops operand
      (4,_)       -> process (M.insert 'B' res reg) ops (pointer + 2)
                     where res = (reg M.! 'B') `xor` (reg M.! 'C')
      (5,operand) -> ((combo reg operand) `mod` 8):(process reg ops (pointer + 2))
      (6,operand) -> process (M.insert 'B' res reg) ops (pointer + 2)
                     where res = (reg M.! 'A') `div` (2 ^ (combo reg operand))
      (7,operand) -> process (M.insert 'C' res reg) ops (pointer + 2)
                     where res = (reg M.! 'A') `div` (2 ^ (combo reg operand))
      _ -> error "unrecognised op"

toOctal :: Int -> [Int]
toOctal x = reverse $ go x
    where go 0 = []
          go a = (a `mod` 8):(go $ a `div` 8)

