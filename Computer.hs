module Computer
( Computer
, Program
, Instruction
, Operator
)
where
import Common

-- DATA DEFINITION

data Computer = Computer { cpu :: CPU
                         , mem :: Memory
                         } deriving (Show, Eq)

data CPU = CPU { ops :: [NamedOp]
               , regs :: [Register]
               } deriving (Show, Eq)

data Memory = RAM [Int] deriving (Show, Eq)

data Register = REG { name :: String
                    , val :: Int
                    } deriving (Show, Eq)

data Program = Program [Instruction] deriving (Show)

-- An Instruction is a specification of how to execute an Operation
data Instruction = Void String              -- Name, no params
                 | Unary String Int         -- Name, Int param
                 | Binary String (Int, Int) -- Name, Two int params
                 deriving (Show, Eq) 

data NamedOp = NamedOp String Operator

instance Show NamedOp where
    show (NamedOp s o) = show s

instance Eq NamedOp where
    (NamedOp a _) == (NamedOp b _) = a == b

data Operator = VoidOp ([Register] -> [Register])
              | UnaryOp ([Register] -> Int -> [Register])
              | BinaryOp ([Register] -> (Int,Int) -> [Register])

instance Show Operator where
    show (VoidOp f) = show "VoidOp"
    show (UnaryOp f) = show "UnaryOp"
    show (BinaryOp f) = show "BinaryOp"

-- REGISTER OPERATIONS

-- Add to a register value
add :: Int -> [Register] -> Int -> [Register]
add regi rs valAdd = modifyN regi newReg rs where
    newReg = REG { name=(name oldReg), val=((val oldReg)+valAdd) } 
    oldReg = rs !! regi

-- Set a register value
set :: Int -> [Register] -> Int -> [Register]
set regi rs valSet = modifyN regi newReg rs where
    newReg = REG { name=(name oldReg), val=valSet }
    oldReg = rs !! regi

-- jmp ops, specifies zero (PC) register
jmpAbs = set 0
jmpRel = add 0

-- Go to next operation
nextOp = flip jmpRel 1

-- SYSTEM IMPLEMENTATION
exec :: Computer -> Operator -> [Int] -> Computer
exec comp (VoidOp f) _ = execVoid comp (VoidOp f)
exec comp (UnaryOp f) s = if length s == 1
                          then execUnary comp (UnaryOp f) (head s)
                          else error "bad arguments"
exec comp (BinaryOp f) (x:xs) = if length xs == 1
                                then execBinary comp (BinaryOp f) (x, head xs)
                                else error "bad arguments"

execNamed :: Computer -> String -> [Int] -> Computer
execNamed (Computer (CPU ops regs) mem) name args = new where
    new = exec old (findOp ops name) args
    old = (Computer (CPU ops regs) mem)

execVoid :: Computer -> Operator -> Computer
execVoid (Computer (CPU ops regs) mem) (VoidOp f) = comp where
    comp = Computer (CPU ops (f regs)) mem
execVoid _ _ = error "Not a void operator"

execUnary :: Computer -> Operator -> Int -> Computer
execUnary (Computer (CPU ops regs) mem) (UnaryOp f) arg = comp where
    comp = Computer (CPU ops (f regs arg)) mem
execUnary _ _ _ = error "Not a unrary operator"

execBinary :: Computer -> Operator -> (Int,Int) -> Computer
execBinary (Computer (CPU ops regs) mem) (BinaryOp f) arg = comp where
    comp = Computer (CPU ops (f regs arg)) mem
execBinary _ _ _ = error "Not a unrary operator"

findOp :: [NamedOp] -> String -> Operator
findOp [] _ = error "bad operator"
findOp ((NamedOp name op):xs) fName = if name == fName
                                      then op
                                      else findOp xs name

-- SYSTEM SETUP

sysOps = [
    NamedOp "nop" (VoidOp nextOp),
    NamedOp "acc" (UnaryOp (add 1))]

sysRegs = [
    REG { name="pc", val=0 },  -- Program Counter
    REG { name="acc", val=0 }] -- Accumulator

system = Computer { cpu=CPU { ops=sysOps, regs=sysRegs }
                  , mem=RAM []
                  }

emptyCptr = Computer { cpu=CPU { ops=[], regs=[] }
                     , mem=RAM []
                     }

-- parseProgram :: [String] -> Program
