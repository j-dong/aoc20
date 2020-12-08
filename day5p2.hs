import Control.Monad
import Control.Arrow
import System.IO
import Data.Maybe
import Data.List
import Data.Char
import Text.Read

filename = "day5_input"

seatId :: (Int, Int) -> Int
seatId (r, c) = 8 * r + c

wholeRange :: ((Int, Int), (Int, Int))
wholeRange = ((0, 128), (0, 8))

bisect ((rlo, rhi), (clo, chi)) c =
    case c of
        'F' -> ((rlo, rmi), (clo, chi))
        'B' -> ((rmi, rhi), (clo, chi))
        'L' -> ((rlo, rhi), (clo, cmi))
        'R' -> ((rlo, rhi), (cmi, chi))
    where rmi = (rlo + rhi) `div` 2
          cmi = (clo + chi) `div` 2

parseSeat :: String -> (Int, Int)
parseSeat = (fst *** fst) . foldl' bisect wholeRange

pairs xs = zip xs (tail xs)

process = putStrLn . show
        . (+1) . fst
        . head
        . filter (\(x, y) -> y == x + 2)
        . pairs
        . sort
        . map (seatId . parseSeat)
        . lines

main = readFile filename >>= process
