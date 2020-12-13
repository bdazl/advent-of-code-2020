module Computer
( Computer (Computer, cpu, mem)
, exited
, pc, acc
, CPU (CPU, ops, regs)
, Memory (RAM)
, Register (REG, rname, val)
, NamedOp (NamedOp)
, Operator (VoidOp, UnaryOp, BinaryOp)
, Program (Program)
, Instruction (Instruction, iname, args, op)
, run
, step
, exec
, execNamed
, execInstr
, findOp
, sysOps
, sysRegs
, system
)
where

import Common

-- DATA DEFINITION

data Computer = Computer { cpu :: CPU
                         , mem :: Memory
                         } deriving (Show, Eq)

exited :: Computer -> Program -> Bool
exited comp (Program prog) = length prog <= pc comp

-- Value of registers
pc :: Computer -> Int
pc (Computer (CPU _ regs) _) = ((val . head) regs)

acc :: Computer -> Int
acc (Computer (CPU _ regs) _) = ((val . (!!1)) regs)

data CPU = CPU { ops :: [NamedOp]
               , regs :: [Register]
               } deriving (Show, Eq)

data Memory = RAM [Int] deriving (Show, Eq)

data Register = REG { rname :: String
                    , val :: Int
                    } deriving (Show, Eq)

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

-- Sets of instructions
data Program = Program [Instruction] deriving (Show, Eq)

-- An Instruction is a specification of how to execute an Operation
data Instruction = Instruction { iname :: String 
                               , args :: [Int]
                               , op :: Operator
                               } deriving (Show)
instance Eq Instruction where
    (Instruction n0 a0 o0) == (Instruction n1 a1 o1) = n0 == n1 && a0 == a1


-- SYSTEM IMPLEMENTATION

-- (Potentially) infinite list of stepped computers
run :: Computer -> Program -> [Computer]
run c p = o where
    o = if exited c p
        then []
        else stp : run stp p where
            stp = step c p

-- Step one instruction
step :: Computer -> Program -> Computer
step (Computer (CPU ops regs) mem) (Program prog) = new where
    new = execInstr old (prog !! ((val . head) regs))
    old = (Computer (CPU ops regs) mem)


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

execInstr :: Computer -> Instruction -> Computer
execInstr (Computer (CPU ops regs) mem) (Instruction name args op) = new where
    new = exec old op args
    old = (Computer (CPU ops regs) mem)

execVoid :: Computer -> Operator -> Computer
execVoid (Computer (CPU ops regs) mem) (VoidOp f) = comp where
    comp = Computer (CPU ops (f regs)) mem
execVoid _ _ = error "Not a void operator"

execUnary :: Computer -> Operator -> Int -> Computer
execUnary (Computer (CPU ops regs) mem) (UnaryOp f) arg = comp where
    comp = Computer (CPU ops (f regs arg)) mem
execUnary _ _ _ = error "Not a unary operator"

execBinary :: Computer -> Operator -> (Int,Int) -> Computer
execBinary (Computer (CPU ops regs) mem) (BinaryOp f) arg = comp where
    comp = Computer (CPU ops (f regs arg)) mem
execBinary _ _ _ = error "Not a unary operator"

findOp :: [NamedOp] -> String -> Operator
findOp [(NamedOp nam op)] s = if nam == s
                              then op
                              else error "bad operator"
findOp ((NamedOp nam op):xs) fName = if nam == fName
                                     then op
                                     else findOp xs fName

-- REGISTER OPERATIONS

-- Add to a register value
add :: Int -> [Register] -> Int -> [Register]
add regi rs valAdd = modifyN regi newReg rs where
    newReg = REG { rname=(rname oldReg), val=((val oldReg)+valAdd) } 
    oldReg = rs !! regi

-- Set a register value
set :: Int -> [Register] -> Int -> [Register]
set regi rs valSet = modifyN regi newReg rs where
    newReg = REG { rname=(rname oldReg), val=valSet }
    oldReg = rs !! regi

-- jmp ops, specifies zero (PC) register
jmpAbs = set 0
jmpRel = add 0

-- Go to next operation
nextOp = flip jmpRel 1

-- SYSTEM SETUP

sysOps = [
    NamedOp "nop" (VoidOp nextOp),
    NamedOp "acc" (UnaryOp (\x -> nextOp . (add 1 x))),
    NamedOp "jmp" (UnaryOp jmpRel)]

sysRegs = [
    REG { rname="pc", val=0 },  -- Program Counter
    REG { rname="acc", val=0 }] -- Accumulator

system = Computer { cpu=CPU { ops=sysOps, regs=sysRegs }
                  , mem=RAM []
                  }
