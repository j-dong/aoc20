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

filename = "day11_input"

converge :: Eq a => (a -> a) -> a -> a
converge f x = let y = f x in if x == y then y else converge f y

do3 f (x, y, z) = (f x, f y, f z)

evolve rows' = map (map process) neighbors where
    process ((a, b, c), (d, e, f), (g, h, i)) = let
            neighbors = [a, b, c, d, f, g, h, i]
            neighborSum z = length . filter (== z) $ neighbors
        in case e of
            'L' -> if neighborSum '#' == 0 then '#' else 'L'
            '#' -> if neighborSum '#' >= 4 then 'L' else '#'
            x -> x where
    neighbor_row :: [Char] -> [(Char, Char, Char)]
    neighbor_row row' = zip3 row (tail row) (drop 2 row) where
        row = "." ++ row' ++ "."
    neighbors :: [[((Char, Char, Char), (Char, Char, Char), (Char, Char, Char))]]
    neighbors = zipWith3 zip3 rows (tail rows) (drop 2 rows) where
        rows :: [[(Char, Char, Char)]]
        rows = map neighbor_row ([filler] ++ rows' ++ [filler])
    width = length (head rows')
    filler = (take width . repeat $ '.')

process = putStrLn . show
        . length . filter (== '#')
        . join
        . converge evolve
        . lines

main = readFile filename >>= process
