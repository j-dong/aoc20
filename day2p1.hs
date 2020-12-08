import Control.Monad
import Control.Arrow
import System.IO
import Data.Maybe
import Data.List
import Text.Read

filename = "day2_input"

data Policy = Policy Char Int Int
    deriving (Show)

split1 :: Char -> String -> (String, String)
split1 sep = second (drop 1) . break (== sep)

inRange :: Ord a => (a, a) -> a -> Bool
inRange (lo, hi) n = n >= lo && n <= hi

only :: [a] -> Maybe a
only [a] = Just a
only _ = Nothing

parseLine :: String -> Maybe (Policy, String)
parseLine s = do
    let (pol, pass) = second (drop 1) $ split1 ':' s
    let (range, char') = split1 ' ' pol
    let (lo', hi') = split1 '-' range
    char <- only char'
    lo <- readMaybe lo'
    hi <- readMaybe hi'
    return (Policy char lo hi, pass)

matches :: Policy -> String -> Bool
matches (Policy c lo hi) = inRange (lo, hi) . length . filter (== c)

process :: String -> IO ()
process = putStrLn . show . length
        . filter (uncurry matches)
        . map (fromJust . parseLine)
        . lines

main = readFile filename >>= process
