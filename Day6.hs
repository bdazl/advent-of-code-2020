import Common
import Data.Char (isSpace)
import Data.Set (Set)
import qualified Data.Set as Set

part1 :: [String] -> Int
part1 s = y where
    y = sum len
    norm = normalize s
    sets = map Set.fromList norm
    len = map Set.size sets

trim :: String -> String
trim = f . f where
    f = reverse . dropWhile isSpace

normalize :: [String] -> [String]
normalize s = rec "" s where
    rec :: String -> [String] -> [String]
    rec s [] = [s]
    rec s (x:xs) = if x == ""
                   then [trim s] ++ rec "" xs
                   else rec (s++x) xs

solve :: [String] -> Int
solve = part1

mini = [
    "abc",
    "",
    "a", "b", "c",
    "",
    "ab", "ac",
    "",
    "a", "a", "a", "a",
    "",
    "b"]

main = do mapStringsFromFile solve "day6.txt"
