import Common

data State = Walk | Empty | Occupied
             deriving (Eq, Show)

type Point = (Int, Int)

main = mapStringsFromFile solve "day11.txt"

solve = part1
part1 :: [String] -> Int
part1 = (stateCount Occupied) . evolvedrain . mapState

toState :: Char -> State
toState '.' = Walk
toState 'L' = Empty
toState '#' = Occupied

toChar :: State -> Char
toChar Walk     = '.'
toChar Empty    = 'L'
toChar Occupied = '#'

mapState :: [String] -> [[State]]
mapState = map (map toState)

mapStrings :: [[State]] -> [String]
mapStrings = map (map toChar)

stateCount :: State -> [[State]] -> Int
stateCount s = sum . (map (count (==s)))

evolvedrain :: [[State]] -> [[State]]
evolvedrain xss = if stepped == xss
                  then xss
                  else evolvedrain stepped
                  where
                  stepped = evolvestep xss

-- evolve state given neighbours
evolve :: State -> [State] -> State
evolve Walk _ = Walk
evolve Empty xs = if count (==Occupied) xs == 0
                  then Occupied
                  else Empty
evolve Occupied xs = if count (==Occupied) xs >= 4
                     then Empty
                     else Occupied

evolvestep :: [[State]] -> [[State]]
evolvestep xss = evlv (0,0) xss
                 where
                 evlv :: Point -> [[State]] -> [[State]]
                 evlv _ []            = []
                 evlv (c, r) (y:yss)  = evrow (c,r) y : evlv (c,r+1) yss

                 evrow :: Point -> [State] -> [State]
                 evrow _ []           = []
                 evrow (c, r) (y:yss) = evolve y (cutout (c,r) xss) : evrow (c+1,r) yss

-- Cut out neighbours and state, from point and 2d board of states
cutout :: Point -> [[State]] -> [State]
cutout (x,y) xss = prow ++ row ++ nrow
                   where
                   prow = (cutoutMid x) . sslast $ upp
                   row  = (cutoutRow x) . sshead $ low
                   nrow = (cutoutMid x) . sshead $ drop 1 low
                   (upp, low) = splitN y xss

cutoutMid :: Int -> [State] -> [State]
cutoutMid n xs = out 
                 where
                 (_, out) = splitN (n-1) lhs
                 (lhs, _) = splitN (n+2) xs

cutoutRow :: Int -> [State] -> [State]
cutoutRow n xs = lasts lhs ++ heads (drop 1 rhs)
                 where
                 (lhs, rhs) = splitN n xs


lasts :: [a] -> [a]
lasts [] = []
lasts xs = [last xs]

heads :: [a] -> [a]
heads [] = []
heads xs = [head xs]

sslast :: [[a]] -> [a]
sslast [] = []
sslast xss = last xss

sshead :: [[a]] -> [a]
sshead [] = []
sshead xss = head xss

smalls = mapState small
small = ["L.LL.LL.LL",
         "LLLLLLL.LL",
         "L.L.L..L..",
         "LLLL.LL.LL",
         "L.LL.LL.LL",
         "L.LLLLL.LL",
         "..L.L.....",
         "LLLLLLLLLL",
         "L.LLLLLL.L",
         "L.LLLLL.LL"]

iter1 = ["#.##.##.##",
         "#######.##",
         "#.#.#..#..",
         "####.##.##",
         "#.##.##.##",
         "#.#####.##",
         "..#.#.....",
         "##########",
         "#.######.#",
         "#.#####.##"]

