import Control.Monad
import System.IO
import Data.Maybe
import Data.List
import Text.Read

filename = "day1_input"
target_sum = 2020

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs

triplets :: [a] -> [(a, a, a)]
triplets [] = []
triplets (x:xs) = map (\(y, z) -> (x, y, z)) (pairs xs) ++ triplets xs

process :: String -> Maybe Int
process = fmap (\(a, b, c) -> a * b * c)
        . listToMaybe
        . filter (\(a, b, c) -> a + b + c == target_sum)
        . triplets
        . catMaybes
        . map readMaybe
        . lines

main = readFile filename >>= (putStrLn . show . fromJust . process)
