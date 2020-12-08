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

parseLine :: String -> (String, [(Int, String)])
parseLine s = (intercalate " " from, map (second (intercalate " ")) to) where

    (from, to) = parseFirstColor (words s)

    parseFirstColor :: [String] -> ([String], [(Int, [String])])
    parseFirstColor ("bags":"contain":ws) = ([], parseColors ws)
    parseFirstColor (w:ws) = first (w:) (parseFirstColor ws)

    parseColors :: [String] -> [(Int, [String])]
    parseColors ["no", "other", "bags."] = []
    parseColors [] = []
    parseColors (num:ws) = ((read num, bag) : parseColors rest)
                          where (bag, rest) = parseColor ws

    parseColor :: [String] -> ([String], [String])
    parseColor (w:ws)
        | w `elem` ["bag,", "bags,", "bag.", "bags."] = ([], ws)
        | otherwise = first (w:) (parseColor ws)

type Graph = Map.Map String [(Int, String)]

addEdge :: String -> (Int, String) -> Graph -> Graph
addEdge from to = Map.alter (Just . (to:) . fromMaybe []) from

addEdges :: (String, [(Int, String)]) -> Graph -> Graph
addEdges (from, tos) = ($ tos) . foldr (addEdge from)

treeSum :: Graph -> String -> Int
treeSum g k = 1 + sum (map (\(num, color) -> num * treeSum g color) (Map.findWithDefault [] k g))

process = putStrLn . show
        . (\g -> treeSum g initial - 1)
        . foldr addEdges Map.empty
        . map parseLine
        . lines

main = readFile filename >>= process
