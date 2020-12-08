import Control.Monad
import Control.Arrow
import System.IO
import Data.Maybe
import Data.List
import Data.Char
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Text.Read

filename = "day7_input"

initial = "shiny gold"

parseLine :: String -> (String, [String])
parseLine s = (intercalate " " from, map (intercalate " ") to) where

    (from, to) = parseFirstColor (words s)

    parseFirstColor :: [String] -> ([String], [[String]])
    parseFirstColor ("bags":"contain":ws) = ([], parseColors ws)
    parseFirstColor (w:ws) = first (w:) (parseFirstColor ws)

    parseColors :: [String] -> [[String]]
    parseColors ["no", "other", "bags."] = []
    parseColors [] = []
    -- seq here to make sure num is actually a number so we can make sure parsing works
    parseColors (num:ws) = seq (read num :: Int) (bag : parseColors rest)
                          where (bag, rest) = parseColor ws

    parseColor :: [String] -> ([String], [String])
    parseColor (w:ws)
        | w `elem` ["bag,", "bags,", "bag.", "bags."] = ([], ws)
        | otherwise = first (w:) (parseColor ws)

type Graph = Map.Map String [String]

addEdge :: String -> String -> Graph -> Graph
addEdge from to = Map.alter (Just . (to:) . fromMaybe []) from where

addEdges :: (String, [String]) -> Graph -> Graph
addEdges (to, froms) = ($ froms) . foldr (flip addEdge to)

dfs :: Graph -> Set.Set String -> [String] -> Set.Set String
dfs g = foldr f where
    f :: String -> Set.Set String -> Set.Set String
    f u visited = if u `Set.member` visited
                      then visited'
                      else dfs g visited' (Map.findWithDefault [] u g)
                  where visited' = Set.insert u visited

process = putStrLn . show . Set.size
        . Set.delete initial
        . (\g -> dfs g Set.empty [initial])
        . foldr addEdges Map.empty
        . map parseLine
        . lines

main = readFile filename >>= process
