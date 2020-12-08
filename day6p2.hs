import Control.Monad
import Control.Arrow
import System.IO
import Data.Maybe
import Data.List
import Data.Char
import qualified Data.Set as Set
import Text.Read

filename = "day6_input"

splitP :: (a -> Bool) -> [a] -> [[a]]
splitP _ [] = []
splitP p xs = group : (splitP p (dropWhile p rest))
             where (group, rest) = break p xs

split :: Eq a => a -> [a] -> [[a]]
split d = splitP (== d)

process = putStrLn . show . sum
        . map (Set.size . foldr1 Set.intersection . map Set.fromList)
        . split ""
        . lines

main = readFile filename >>= process
