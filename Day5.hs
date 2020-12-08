import Common

rpart a b = (div (b-a) 2) + a
lpart a b = (div (b-a) 2) + a + 1

part (a,b) lower = if lower
                   then rpart a b
                   else lpart a b

shrink :: (Int,Int) -> Bool -> (Int,Int)
shrink (a,b) lower = if lower
                     then (a, rpart a b)
                     else (lpart a b, b)

lowerMap :: Char -> Bool
lowerMap 'F' = True
lowerMap 'B' = False
lowerMap 'R' = False
lowerMap 'L' = True
lowerMap s = error "bad char"

foldShrink :: (Int, Int) -> [Bool] -> (Int, Int)
foldShrink inp [] = inp
foldShrink inp (x:xs) = foldShrink (shrink inp x) xs

mapLowerBool s = map lowerMap s

takeRow s = take 7 s
takeCol s = reverse (take 3 (reverse s))

partAlgo :: (Int,Int) -> String -> Int
partAlgo tupl s = y where 
    y = part initPart (last bools)
    initPart = foldShrink tupl (init bools)
    bools = mapLowerBool s

getPos :: String -> (Int,Int)
getPos s = (row,col) where
    row = partAlgo (0,127) (takeRow s)
    col = partAlgo (0,7)   (takeCol s)

ident :: String -> Int
ident s = y where
    y = row*8 + col
    (row,col) = getPos s

part1 :: [String] -> Int
part1 x = y where
    y = maximum ids
    ids = map ident x

addSeat :: (Int,Int) -> [[Bool]] -> [[Bool]]
addSeat (row,col) l = y where
    y = modifyN row nrow l

    nrow = modifyN col True _row
    _row = l !! row


populateSeats :: [(Int, Int)] -> [[Bool]]
populateSeats s = y where
    y = rec s base
    base = [replicate 8 False | _ <- [0..127]]

    rec :: [(Int, Int)] -> [[Bool]] -> [[Bool]]
    rec [] start = start
    rec (x:xs) start = addSeat x start


part2 :: [String] -> [[Bool]]
part2 x = y where
    y = populateSeats ids
    ids = map getPos x

solve :: [String] -> Int
solve = part1


test = solve mini
mini = ["BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL"]

main = do mapStringsFromFile solve "day5.txt"
