import Common

isTree :: Char -> Bool
isTree c = c == '#'

rowIsTree :: Int -> String -> Bool
rowIsTree i s  = (isTree . (!! (i*3))) (cycle s)

tst :: [Int] -> Int
tst x = sum x

solve :: [String] -> Int
solve s = y where
    y = length (filter (==True) treeRows)
    treeRows = [rowIsTree (fromIntegral a) b | (a,b) <- enumerate s]

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
