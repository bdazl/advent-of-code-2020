import Common

goodReq :: String -> (Bool,Bool)
goodReq "byr" = (True, True)
goodReq "iyr" = (True, True)
goodReq "eyr" = (True, True)
goodReq "hgt" = (True, True)
goodReq "hcl" = (True, True)
goodReq "ecl" = (True, True)
goodReq "pid" = (True, True)
goodReq "cid" = (True, False)
goodReq s     = (False,False)

distortOnFail (a,b) = a * (1-b)

solve :: [String] -> Int
solve s = y where
    y = length (filter (==7) conc)

    -- hack to make sure that any key that is not valid will
    -- distort the result in such a way that the condition fails
    conc = map (\(a,b) -> a * (1-b)) (zip goodsum nogsum)

    norm   = normalize s
    split  = map (splitOn ' ') norm

    goreqTupl  = map (map (goodReq . head . (splitOn ':'))) split

    good  = map (filter ((==True) . fst)) goreqTupl
    goodAndReq = map (filter and) good
    goodsum = map length goodAndReq

    nogood = map (filter ((==False) . fst)) goreqTupl
    nogsum = map length nogood    


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
