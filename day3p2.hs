import Control.Monad
import Control.Arrow
import System.IO
import Data.Maybe
import Data.List
import Text.Read

filename = "day3_input"

onlyMultiples :: Int -> [a] -> [a]
onlyMultiples m = doFilter 0
    where doFilter 0 (x:xs) = x : (doFilter (m - 1) xs)
          doFilter i (x:xs) = doFilter (i - 1) xs
          doFilter _ [] = []

processLine :: Int -> Int -> String -> Int
processLine mult i = oneIfTree . head . drop (mult * i) . cycle
    where oneIfTree '#' = 1
          oneIfTree  _  = 0

processSlope :: (Int, Int) -> [String] -> Int
processSlope (dx, dy) = sum
                      . drop 1
                      . map (uncurry (processLine dx))
                      . zip [0..]
                      . onlyMultiples dy

process :: String -> IO ()
process x = putStrLn . show
          . product
          . map (flip processSlope (lines x))
          $ [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main = readFile filename >>= process
