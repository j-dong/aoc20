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

filename = "day8_input"

data State = State { acc :: Int, pc :: Int }
    deriving (Show)
type Instruction = State

(>+>) :: State -> State -> State
st1 >+> st2 = State { acc = (acc st1) + (acc st2), pc = (pc st1) + (pc st2) }

readSigned :: String -> Int
readSigned ('+':num) = read num
readSigned ('-':num) = -(read num)

parseInstruction :: [String] -> Maybe Instruction
parseInstruction ("acc":num:[]) = Just (State { acc=readSigned num, pc=1 })
parseInstruction ("jmp":num:[]) = Just (State { acc=0, pc=readSigned num })
parseInstruction ("nop":num:[]) = Just (State { acc=0, pc=1 })
parseInstruction _ = Nothing

doPart1 :: [Instruction] -> Int
doPart1 insts = acc . snd . fromJust $ result where
    result = find (\(visited, state) -> pc state `Set.member` visited) steps
    steps = iterate evolve initial_state
    initial_state = (Set.empty, (State { acc=0, pc=0 }))
    prog = listArray (0, length insts - 1) insts
    evolve (visited, state) = let cur = pc state in
        (Set.insert cur visited, state >+> (prog ! cur))

process = putStrLn . show . doPart1
        . catMaybes
        . map (parseInstruction . words)
        . lines

main = readFile filename >>= process
