import Common

valid1 :: String -> String -> Bool
valid1 "byr" v = True
valid1 "iyr" v = True
valid1 "eyr" v = True
valid1 "hgt" v = True
valid1 "hcl" v = True
valid1 "ecl" v = True
valid1 "pid" v = True
valid1 s _     = False

valid2 :: String -> String -> Bool
valid2 "byr" v = within 1920 2002 (atoi v)
valid2 "iyr" v = within 2010 2020 (atoi v)
valid2 "eyr" v = within 2020 2030 (atoi v)
valid2 "hgt" v = True
valid2 "hcl" v = True
valid2 "ecl" v = 1 == (length (filter (==v) validecl))
valid2 "pid" v = length v == 9 && allDigits v
valid2 s _     = False

validecl = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validate :: (String -> String -> Bool) -> (String, String) -> Bool
validate valid x = valid (fst x) (snd x)

within :: Int -> Int -> Int -> Bool
within a b x = a <= x && x <= b

listAnd l = sum (map btoi l) == length l
digits l = map isDigit l
isDigit = (flip elem) ['0'..'9']

allDigits l = listAnd (digits l)

tuplSplit (a,b) = a b
tuplConv x = (head x, last x)
notCid x = fst x /= "cid"
tuples = tuplConv . (splitOn ':')
filterCids x = filter notCid (map tuples x)

partSolve :: (String -> String -> Bool) -> [String] -> Int
partSolve valid s = y where
    y = length (filter (==7) sumValid)

    sumValid = map (sum . (map btoi)) validFields
    validFields = map (map (validate valid)) cidsRemoved

    cidsRemoved = map filterCids split

    norm   = normalize s
    split  = map (splitOn ' ') norm

solve :: [String] -> Int
solve s = partSolve valid2 s

join :: String -> String -> String
join a b = if a == ""
           then b
           else a ++ " " ++ b

normalize :: [String] -> [String]
normalize s = normalizer "" s

normalizer :: String -> [String] -> [String]
normalizer s [] = [s]
normalizer s (x:xs) = if x == ""
                      then [s] ++ normalizer "" xs
                      else normalizer (join s x) xs

mini = id ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
           "byr:1937 iyr:2017 cid:147 hgt:183cm",
           "",
           "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
           "hcl:#cfa07d byr:1929",
           "",
           "hcl:#ae17e1 iyr:2013",
           "eyr:2024",
           "ecl:brn pid:760753108 byr:1931",
           "hgt:179cm",
           "",
           "hcl:#cfa07d eyr:2025 pid:166559648",
           "iyr:2011 ecl:brn hgt:59in"]

test = solve mini
main = do mapStringsFromFile solve "day4.txt"
