import Control.Monad
import Control.Arrow
import System.IO
import Data.Maybe
import Data.List
import Data.Char
import Data.Array
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Text.Read
import Debug.Trace

filename = "day10_input"

doPart2 = snd . head . foldr go [] where
    go :: Int -> [(Int, Int)] -> [(Int, Int)]
    go x [] = [(x, 1)]
    go x l = (x, sum . map snd . takeWhile (\(y, _) -> y - x <= 3) $ l) : l

process = putStrLn . show
        . doPart2
        . (0 :)
        . (\l -> l ++ [3 + last l])
        . sort
        . map read
        . lines

main = readFile filename >>= process
