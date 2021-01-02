import Data.List
import Common
import Data.Map (Map)
import qualified Data.Map as Map

main = do mapIntsFromFile solve "day10.txt"

solve = part2

part2 :: [Int] -> Int
part2 = head . revBranches . reverse . reachMap . normalize

part1 :: [Int] -> Int
part1 ns = diff1 * diff3
           where
           diff1 = count (==1) diffed
           diff3 = count (==3) diffed
           diffed = diffs . normalize $ ns

revBranches :: [Int] -> [Int]
revBranches ns = recurse ns []
                     where
                     recurse :: [Int] -> [Int] -> [Int]
                     recurse [] ls = ls
                     recurse (n:ns) [] = recurse ns [1]
                     recurse (n:ns) aux = recurse ns (sum (take n aux) : aux)

-- How far does each element reach
reachMap :: [Int] -> [Int]
reachMap ns = map (\(a,b) -> reaches a b) nreduce
              where
              nreduce = zip ns reduced
              reduced = map drp (zip [1..] ns)
    
              drp :: (Integer, Int) -> [Int]
              drp (n, _) = drop (fromIntegral n) ns

filterSorted :: (a -> Bool) -> [a] -> [a]
filterSorted _ [] = []
filterSorted pred (x:xs) = if pred x
                           then x : filterSorted pred xs
                           else []

-- Count how many n can reach in list
reaches :: Int -> [Int] -> Int
reaches n  = length . filterSorted (<=3) . (map (\a -> a - n))

 -- normalize input (adds the 0 adapter to a the sorted input and at the end adds bigger adapter)
normalize :: [Int] -> [Int]
normalize ns = 0:sorted ++ [last sorted + 3] where
               sorted = sort ns

diffs :: [Int] -> [Int]
diffs ns = (map (\(a,b) -> b - a)) . pairs $ ns

-- [1, 2, 3] -> [(1, 2), (2, 3)]
pairs :: [Int] -> [(Int, Int)]
pairs [n] = []
pairs (n0:n1:ns) = (n0, n1) : pairs (n1:ns)

small = [28, 33, 18, 42, 31, 14, 46, 20,
         48, 47, 24, 23, 49, 45, 19, 38,
         39, 11, 1, 32, 25, 35, 8, 17, 7,
         9, 4, 2, 34, 10, 3]

mini = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]

test = part1 mini == (7*5) && part1 small == (22*10)
