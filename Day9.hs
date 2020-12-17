import Common
import Data.List
import Data.Maybe

verifyPreamble :: Int -> [Int] -> Bool
verifyPreamble val preamble = isJust found where
    found = find (==val) sums
    sums = map (\x -> fst x + snd x) tupls
    tupls = tuples preamble

verifyPacket :: [Int] -> [Int] -> [Bool]
verifyPacket _ [] = []
verifyPacket preamble (x:xs) = y where
    y = verifyPreamble x preamble : verifyPacket (tail preamble ++ [x]) xs

verify :: Int -> [Int] -> [(Bool, Int)]
verify preN lst = o where
    o = zip valid rhs
    valid = verifyPacket preamble rhs
    (preamble, rhs) = splitN preN lst

firstInvalid :: Int -> [Int] -> Maybe Int
firstInvalid preN lst = case find (\(a,b) -> not a) (verify preN lst) of
                        Nothing -> Nothing
                        Just (a,b) -> Just b

deflate :: Maybe Int -> Int
deflate Nothing = error "could not find an error"
deflate (Just a) = a

part1 :: [Int] -> Int
part1 i = deflate (firstInvalid 25 i)

-- tuples [a, b, c] = [(a, b), (a, c), (b, c)]
tuples :: [Int] -> [(Int,Int)]
tuples [] = []
tuples [m, n] = [(m, n)]
tuples (x:xs) = (zipItem x xs) ++ (tuples xs)

-- zipItem x [a, b, c] = [(x, a), (x, b), (x, c)]
zipItem :: Int -> [Int] -> [(Int, Int)]
zipItem x [] = []
zipItem x [n] = [(x, n)]
zipItem x (y:ys) = [(x,y)] ++ zipItem x ys

mini = [35,
        20,
        15,
        25,
        47,
        40,
        62,
        55,
        65,
        95,
        102,
        117,
        150,
        182,
        127,
        219,
        299,
        277,
        309,
        576]

solve = part1
main = do mapIntsFromFile solve "day9.txt"
