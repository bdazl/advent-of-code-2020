import Common
import Compiler
import Computer
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

part2 :: [String] -> Int
part2 raw = y where
    y = acc (last r)

    r = run system correctedProg
    correctedProg = Program (reverse corLines)
    (Program corLines)  = correctFirstJmp (Program (reverse lines))
    (Program lines) = compile raw

    nopInstr = instruction "nop" []

    correctFirstJmp :: Program -> Program
    correctFirstJmp (Program []) = Program []
    correctFirstJmp (Program (x:xs)) = if iname x == "jmp"
                                       then Program (nopInstr:xs)
                                       else Program (x:newLines) where
                                           Program newLines = correctFirstJmp (Program xs)


part1 :: [String] -> Int
part1 raw = y where
    y = acc foundRec

    foundRec = rec IntSet.empty system prog
    prog = compile raw

    rec :: (IntSet.IntSet) -> Computer -> Program -> Computer
    rec iset comp prog = o where
        o = if IntSet.member (pc comp) iset
            then comp
            else rec (IntSet.insert (pc comp) iset) (step comp prog) prog

solve = part2

mini1 = [
    "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -99",
    "acc +1",
    "jmp -4",
    "acc +6"]

mini2 = [
    "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -99",
    "acc +1",
    "nop -4",
    "acc +6"]

main = do mapStringsFromFile solve "day8.txt"
