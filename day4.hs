import Common

validField :: String -> String -> Bool
validField "byr" v = True
validField "iyr" v = True
validField "eyr" v = True
validField "hgt" v = True
validField "hcl" v = True
validField "ecl" v = True
validField "pid" v = True
validField s _     = False

tuplSplit (a,b) = a b
tuplConv x = (head x, last x)
notCid x = fst x /= "cid"
tuples = tuplConv . (splitOn ':')
filterCids x = filter notCid (map tuples x)
validateStrTuple x = validField (fst x) (snd x)

solve :: [String] -> Int
solve s = y where
    y = length (filter (==7) sumValid)

    sumValid = map (sum . (map btoi)) validFields
    validFields = map (map validateStrTuple) cidsRemoved

    cidsRemoved = map filterCids split

    norm   = normalize s
    split  = map (splitOn ' ') norm



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
