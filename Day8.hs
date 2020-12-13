import Common
import Compiler
import Computer
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

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

solve = part1

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

main = do mapStringsFromFile solve "day8.txt"
