import Common

isTree :: Char -> Bool
isTree c = c == '#'

rowIsTree :: Int -> Int -> String -> Bool
rowIsTree dx i s  = (isTree . (!! (i*dx))) (cycle s)

skip :: [a] -> [a]
skip [n] = [n]
skip (x:xs) = x:(skip (tail xs))

descent :: [String] -> Int -> Int
descent s dx = y where
    y = length (filter (==True) treeRows)
    treeRows = [rowIsTree dx (fromIntegral a) b | (a,b) <- enumerate s]

solve :: [String] -> Int
solve s = y where
    y = product ((descent (skip s) 1):simpl)
    simpl = map (descent s) [1, 3, 5, 7]

mini = id ["..##.......",
           "#...#...#..",
           ".#....#..#.",
           "..#.#...#.#",
           ".#...##..#.",
           "..#.##.....",
           ".#.#.#....#",
           ".#........#",
           "#.##...#...",
           "#...##....#",
           ".#..#...#.#"]

test = solve mini
main = do mapStringsFromFile solve "day3.txt"
