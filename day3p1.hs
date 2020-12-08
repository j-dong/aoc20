import Control.Monad
import Control.Arrow
import System.IO
import Data.Maybe
import Data.List
import Text.Read

filename = "day3_input"

processLine :: Int -> String -> Int
processLine mult i = oneIfTree . head . drop (mult * i) . cycle
    where oneIfTree '#' = 1
          oneIfTree  _  = 0

process :: String -> IO ()
process = putStrLn . show
        . sum
        . drop 1
        . map (uncurry processLine 1)
        . zip [0..]
        . lines

main = readFile filename >>= process
