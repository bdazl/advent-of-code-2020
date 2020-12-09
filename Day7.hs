import Common
import Data.Set (Set)
import qualified Data.Set as Set

-- A Bag has an identifier and holds to bags as content
data Bag = NoBag
         | Bag [String] Content Content deriving (Show)
data Content = NoContent
             | Full [String] Int deriving (Show)

data Lexer = Lexer [String] 

tokenize :: String -> [String]
tokenize s = y where
    y = map trim split
    split = splitOn ' ' s

parse :: [String] -> Bag
parse s = bag where
    (typ, red0) = splitN 2 s
    (_,   red1) = splitN 2 red0
    (has, red2) = (head red1, tail red1)

    bag = if has == "no"
          then Bag typ NoContent NoContent
          else parseFull typ has red2

parseFull :: [String] -> String -> [String] -> Bag
parseFull typ n0 s = bag where
    bag = Bag typ (Full c0 n0i) (Full c1 n0i)

    n0i = read       n0  :: Int
    n1i = read (head n1) :: Int

    (c0, red0) = splitN 2 s
    (_,  red1) = splitN 1 red0
    (n1, red2) = splitN 1 red1
    (c1, red3) = splitN 2 red2


isPunkt c = c == ',' || c == '.'

trim :: String -> String
trim = f . f where
   f = reverse . dropWhile isPunkt

makeBags :: [String] -> [Bag]
makeBags s = map parse tokens where
    tokens = map tokenize s

bagId :: Bag -> [String]
bagId (Bag s c0 c1) = s

contid :: Content -> [String]
contid NoContent = []
contid (Full s i) = s

lcont :: Bag -> Content
lcont (Bag _ c0 _) = c0

rcont :: Bag -> Content
rcont (Bag _ _ c1) = c1

contEqBag :: Content -> Bag -> Bool
contEqBag NoContent _ = False
contEqBag _ NoBag = False
contEqBag (Full fs _) (Bag bs _ _) = fs == bs

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

deps :: Bag -> [Bag] -> [Bag]
deps NoBag _ = []
deps (Bag s c0 c1) bags = d where
    bag = (Bag s c0 c1)
    orig =  Set.singleton s
    d = bag:(rec orig bags c0) ++ (rec orig bags c1)

    rec :: (Set.Set [String]) -> [Bag] -> Content -> [Bag]
    rec _ _ NoContent = []
    rec seen bags ident = o where
        o = if Set.member (contid ident) seen
            then []
            else found ++ flatten (map leftM found) ++ flatten (map rightM found)
        found = filter (contEqBag ident) bags

        leftM (Bag s c0 _) = r where
            addMe = Set.insert (contid ident) seen
            meB = Set.insert s addMe
            r = rec meB bags c0
        rightM (Bag s _ c1) = r where
            addMe = Set.insert (contid ident) seen
            meB = Set.insert s addMe
            leftBranch = Set.fromList (map bagId found)
            union = Set.union leftBranch meB
            r = rec union bags c1

hasDep :: [Bag] -> [String] -> Bool
hasDep _ [] = False
hasDep [] _ = False
hasDep (x:xs) s = if (bagId x) == s
                  then True
                  else hasDep xs s

part1 :: [String] -> Int
part1 s = sum y where
    y = map btoi found

    myBag = ["shiny", "gold"]
    parsed = map (parse . tokenize) s
    rem = filter ((/=myBag) . bagId) parsed
    dep = map (flip deps parsed) rem
    found = map (flip hasDep myBag) dep

mini = ["light red bags contain 1 bright white bag, 2 muted yellow bags.",
        "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
        "bright white bags contain 1 shiny gold bag.",
        "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
        "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
        "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
        "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
        "faded blue bags contain no other bags.",
        "dotted black bags contain no other bags."]

solve = part1
main = do mapStringsFromFile solve "day7.txt"
