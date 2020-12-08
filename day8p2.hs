import Control.Monad
import Control.Arrow
import System.IO
import Data.Maybe
import Data.List
import Data.Char
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Text.Read
import Debug.Trace

filename = "day8_input"

data State = State { acc :: Int, pc :: Int }
    deriving (Show)
data Instruction = Instruction { mnemonic :: String, instValue :: Int }
    deriving (Show)

(>+>) :: State -> State -> State
st1 >+> st2 = State { acc = (acc st1) + (acc st2), pc = (pc st1) + (pc st2) }

readSigned :: String -> Int
readSigned ('+':num) = read num
readSigned ('-':num) = -(read num)

state' :: Instruction -> State
state' Instruction { mnemonic="acc", instValue=num } = State { acc=num, pc=1 }
state' Instruction { mnemonic="jmp", instValue=num } = State { acc=0, pc=num }
state' Instruction { mnemonic="nop", instValue=num } = State { acc=0, pc=1 }

flipInst :: Instruction -> Instruction
flipInst inst = inst { mnemonic = flipped } where
    flipped = case (mnemonic inst) of
        "acc" -> "acc"
        "jmp" -> "nop"
        "nop" -> "jmp"

parseInstruction :: [String] -> Maybe Instruction
parseInstruction (mne:num:[]) = Just (Instruction { mnemonic=mne, instValue=readSigned num })
parseInstruction _ = Nothing

doPart2 :: [Instruction] -> Int
doPart2 insts = runAndFix (State { pc=0, acc=0 }) where
    proglen = length insts
    prog = listArray (0, proglen - 1) insts
    run state = if pc state == proglen then acc state else run (state >+> state' (prog ! (pc state)))
    runAndFix state = if pc (step flipped) `Set.member` terminating
                          then run $ step flipped
                          else runAndFix $ step curInst
                      where
                          cur = pc state
                          curInst = prog ! cur
                          flipped = flipInst curInst
                          step = (state >+>) . state'
    terminating = dfs (Set.empty, [proglen]) where
        dfs (visited, []) = visited
        dfs (visited, i:is)
            | i `Set.member` visited = dfs (visited, is)
            | otherwise = dfs (Set.insert i visited, revG ! i ++ is)
    revG = accumArray (flip (:)) [] (0, proglen) . map toEntry . zip [0..] . map state' $ insts where
        toEntry (i, inst) = (i + pc inst, i)

process = putStrLn . show . doPart2
        . catMaybes
        . map (parseInstruction . words)
        . lines

main = readFile filename >>= process
