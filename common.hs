module Common
( strToInt
, splitOn
, enumerate
, mapIntsFromFile
, mapStringsFromFile
) where

import System.Environment

enumerate = zip [0..]

strToInt :: String -> Int
strToInt s = read s :: Int

splitOn :: Char -> String -> [String]
splitOn c s = y where 
    y = rec c s []

    rec :: Char -> String -> [String] -> [String]
    rec c "" l = l
    rec c (s:ss) l = rec c ss (elem s c l)

    elem :: Char -> Char -> [String] -> [String]
    elem p c [] = [p:""]
    elem p c l = if p == c
                 then l ++ [""]
                 else (init l) ++ [((last l) ++ [p])]
    

mapIntsFromFile :: Show a => ([Int] -> a) -> String -> IO ()
mapIntsFromFile f fname  = do
    file <- readFile fname
    let l = lines file
    let i = map (strToInt) l
    print (f i)

mapStringsFromFile :: Show a => ([String] -> a) -> String -> IO ()
mapStringsFromFile f fname  = do
    file <- readFile fname
    let l = lines file
    print (f l)
