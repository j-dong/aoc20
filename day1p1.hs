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

process :: String -> Maybe Int
process = fmap (uncurry (*))
        . listToMaybe
        . filter (\(a, b) -> a + b == target_sum)
        . pairs
        . catMaybes
        . map readMaybe
        . lines

main = readFile filename >>= (putStrLn . show . fromJust . process)
