module Common
( atoi
, btoi
, splitN
, modifyN
, splitOn
, count
, enumerate
, mapIntsFromFile
, mapStringsFromFile
) where

import System.Environment

enumerate = zip [0..]

btoi :: Bool -> Int
btoi b = if b
         then 1
         else 0

atoi :: String -> Int
atoi s = read s :: Int

splitN :: Int -> [a] -> ([a], [a])
splitN n l = (take n l, drop n l)

modifyN :: Int -> a -> [a] -> [a]
modifyN n new l = y where
    y = map (\(i,x) -> if i == n then new else x) enum
    enum = zip [0..] l


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
    
-- Given a predicate, count the number of occorences in a list
count :: (a -> Bool) -> [a] -> Int
count pred xs = length . (filter pred) $ xs

mapIntsFromFile :: Show a => ([Int] -> a) -> String -> IO ()
mapIntsFromFile f fname  = do
    file <- readFile fname
    let l = lines file
    let i = map (atoi) l
    print (f i)

mapStringsFromFile :: Show a => ([String] -> a) -> String -> IO ()
mapStringsFromFile f fname  = do
    file <- readFile fname
    let l = lines file
    print (f l)
