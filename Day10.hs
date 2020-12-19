import Data.List
import Common

part1 :: [Int] -> Int
part1 ns = diff1 * diff3
           where
    diff1 = count (==1) diffed
    diff3 = count (==3) diffed
    diffed = diffs . normalize $ ns
 
 -- normalize input (adds the 0 adapter to a the sorted input and at the end adds bigger adapter)
normalize :: [Int] -> [Int]
normalize ns = 0:sorted ++ [last sorted + 3] where
               sorted = sort ns


diffs :: [Int] -> [Int]
diffs ns = (map (\(a,b) -> b - a)) . tuples $ ns

-- [1, 2, 3] -> [(1, 2), (2, 3)]
tuples :: [Int] -> [(Int, Int)]
tuples [n] = []
tuples (n0:n1:ns) = (n0, n1) : tuples (n1:ns)

small = [28, 33, 18, 42, 31, 14, 46, 20,
         48, 47, 24, 23, 49, 45, 19, 38,
         39, 11, 1, 32, 25, 35, 8, 17, 7,
         9, 4, 2, 34, 10, 3]

mini = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]

test = part1 mini == (7*5) && part1 small == (22*10)

solve = part1
main = do mapIntsFromFile solve "day10.txt"
