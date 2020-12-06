module Common
( mapIntsFromFile
) where

import System.Environment

strToInt :: String -> Int
strToInt s = read s :: Int

test :: [Int] -> Int
test x = 1

mapIntsFromFile :: Show a => ([Int] -> a) -> String -> IO ()
mapIntsFromFile f fname  = do
    file <- readFile fname
    let l = lines file
    let i = map (strToInt) l
    print (f i)
