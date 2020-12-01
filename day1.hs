-- zipItem x [a, b, c] = [(x, a), (x, b), (x, c)]
zipItem :: Int -> [Int] -> [(Int, Int)]
zipItem x [] = []
zipItem x [n] = [(x, n)]
zipItem x (y:ys) = [(x,y)] ++ zipItem x ys

-- tuples [a, b, c] = [(a, b), (a, c), (b, c)]
tuples :: [Int] -> [(Int,Int)]
tuples [] = []
tuples [m, n] = [(m, n)]
tuples (x:xs) = (zipItem x xs) ++ (tuples xs)

-- tupleSum [(1, 2), (3, 4)] = [3, 7]
tupleSum :: [(Int, Int)] -> [Int]
tupleSum [(m, n)] = [m+n]
tupleSum ((m, n):xs) = [m+n] ++ tupleSum xs

-- zipTriplet [a] [(b,c)] = [(a,b,c)]
zipTriplet :: [Int] -> [(Int, Int)] -> [(Int, Int, Int)]
zipTriplet [] [] = []
zipTriplet (a:as) ((m,n):xs) = [(a,m,n)] ++ zipTriplet as xs

triplet :: [Int] -> [(Int, Int, Int)]
triplet x = zipTriplet (tupleSum (tuples x)) (tuples x)

firstIsCorrect :: (Int, Int, Int) -> Bool
firstIsCorrect (a, _, _) = a == 2020

findCorrect :: [Int] -> (Int,Int,Int)
findCorrect x = head (filter (firstIsCorrect) (triplet x))

answer :: [Int] -> Int
answer x = y where
    y = b*c
    (a, b, c) = findCorrect x

test = answer [2,200,2000,30,20]
main = print (test)
