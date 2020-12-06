import Common

part1Valid :: (Int,Int) -> Char -> String -> Bool
part1Valid (a,b) c s = y where
    y = a <= l && l <= b
    l = length (filter (==c) s)

part2Valid :: (Int,Int) -> Char -> String -> Bool
part2Valid (a,b) c s = y where
    y = (p1 || p2) && p1 /= p2

    p1 = c == (s !! (a-1))
    p2 = c == (s !! (b-1))


line :: String -> ((Int, Int), Char, String)
line s = (policy,chr,pass) where
    split = splitOn ' ' s

    policy = (policySplit . head) split
    chr = (head . (!! 1)) split
    pass = last split

    policySplit :: String -> (Int, Int)
    policySplit s = (a,b) where 
        ab = splitOn '-' s
        a = strToInt (head ab)
        b = strToInt (last ab)

validStrPart1 :: String -> Bool
validStrPart1 s = y where
    y = part1Valid policy chr pass
    (policy,chr,pass) = line s

validStrPart2 :: String -> Bool
validStrPart2 s = y where
    y = part2Valid policy chr pass
    (policy,chr,pass) = line s


solve1 :: [String] -> Int
solve1 x = y where
    y = length (filter (==True) valid)
    valid = map (validStrPart1) x

solve2 :: [String] -> Int
solve2 x = y where
    y = length (filter (==True) valid)
    valid = map (validStrPart2) x

main = do
    mapStringsFromFile solve2 "day2.txt"
