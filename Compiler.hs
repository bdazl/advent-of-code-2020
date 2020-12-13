module Compiler 
( eval
, compile
)
where
import Computer
import Common

-- Compiler is just a simple assembly compiler (compiles a string into computer operations)

eval :: String -> Instruction
eval s = y where
    y = Instruction { iname=nam
                    , args=map toInt (tail tokens)
                    , op=findOp sysOps nam }
    nam = head tokens
    tokens = tokenize s

compile :: [String] -> Program
compile s = (Program (map eval s))

toInt :: String -> Int
toInt "" = error "bad int"
toInt s = if head s == '+'
          then read (tail s) :: Int
          else read s :: Int


tokenize :: String -> [String]
tokenize s = splitOn ' ' s where
