import Common

valid :: (Int,Int) -> Char -> String -> Bool
valid (a,b) c s = y where
    y = a <= l && l <= b
    l = length (filter (==c) s)

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


validStr :: String -> Bool
validStr s = y where
    y = valid policy chr pass

    split = splitOn ' ' s

    policy = (policySplit . head) split
    chr = (head . (!! 1)) split
    pass = last split

    policySplit :: String -> (Int, Int)
    policySplit s = (a,b) where 
        ab = splitOn '-' s
        a = strToInt (head ab)
        b = strToInt (last ab)

solve :: [String] -> Int
solve x = y where
    y = length (filter (==True) valid)
    valid = map (validStr) x

main = do
    mapStringsFromFile solve "day2.txt"
