module Main where

import qualified Common
import qualified Data.Map as M
import Data.Char (ord)

main :: IO ()
main = do
    putStrLn $ "-- Solving day21 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let passThroughRobotKeypadNTimes n = last . take (n+1) . iterate getShortestRobotPath
        encodingToLength = foldr (\(k,v) -> (+ (length k) * v)) 0 . M.toList
        fromCodeToMyInput nRobots = encodingToLength
                                  . passThroughRobotKeypadNTimes nRobots
                                  . getShortestDoorPath
        toNumber = read . filter (\x -> ord x >= 48 && ord x <= 57)
        solve nRobots = sum
                      $ map (\x -> toNumber x * fromCodeToMyInput nRobots x) parsedInput

    let answer1 = solve 2
    let answer2 = solve 25

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec = (Int,Int)
type Keypad = M.Map Char Vec
type Encoding = M.Map String Int

keypadDoor :: Keypad
keypadDoor = M.fromList [('7',(0,0)), ('8',(1,0)), ('9',(2,0)),
                         ('4',(0,1)), ('5',(1,1)), ('6',(2,1)),
                         ('1',(0,2)), ('2',(1,2)), ('3',(2,2)),
                                      ('0',(1,3)), ('A',(2,3))]
keypadRobot :: Keypad
keypadRobot = M.fromList [('^',(1,0)), ('A',(2,0)),
              ('<',(0,1)),('v',(1,1)), ('>',(2,1))]

parse :: String -> [String]
parse input = lines input

getShortestKeypadPath :: Keypad -> Vec -> Char -> Char -> String
getShortestKeypadPath pad invalid a b
  | a == b = "A"
  | otherwise = if safeToMoveY
                   then yFirst
                   else if safeToMoveX then xFirst else yFirst
  where ((ax,ay),(bx,by)) = Common.mapTuple (pad M.!) (a,b)
        (nx,ny) = (abs (bx-ax),abs (by-ay))
        (dx,tx) = if bx > ax then (1,'>') else (-1,'<') :: (Int,Char)
        ty = if by > ay then 'v' else '^'
        yFirst = (take ny $ repeat ty) ++ (take nx $ repeat tx) ++ "A"
        xFirst = (take nx $ repeat tx) ++ (take ny $ repeat ty) ++ "A"
        safeToMoveY = dx > 0 && (ax,by) /= invalid
        safeToMoveX = (bx,ay) /= invalid

getShortestDoorPath :: String -> Encoding
getShortestDoorPath code = foldr (\token -> M.insertWith (+) token 1) M.empty
                         $ map (uncurry getDoorJump) $ zip ('A':code) code
    where getDoorJump = getShortestKeypadPath keypadDoor (0,3)

getShortestRobotPath :: Encoding -> Encoding
getShortestRobotPath encoding = foldr foldF M.empty $ M.toList encoding
    where getRobotJump = getShortestKeypadPath keypadRobot (0,0)
          tokenToEncoding token = map (uncurry getRobotJump)
                                $ zip ('A':token) token
          foldF (token,count) acc = foldr (\token' -> M.insertWith (+) token' count) acc
                                  $ tokenToEncoding token
