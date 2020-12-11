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

move (dr, dc) (r, c) = (r + dr, c + dc)

lineOfSight grid pos dpos = find (\x -> (grid ! x) /= '.')
                          . takeWhile (inRange (bounds grid))
                          . tail
                          . iterate (move dpos)
                          $ pos

cartesianProduct = liftM2 (,)

linesOfSight grid pos = catMaybes
                      . map (lineOfSight grid pos)
                      . filter (/= (0, 0))
                      $ cartesianProduct [-1..1] [-1..1]

evolve sight grid = array (bounds grid) [(pos, handle pos) | pos <- range (bounds grid)] where
    handle pos = case (grid ! pos) of
                     'L' -> if occupied == 0 then '#' else 'L'
                     '#' -> if occupied >= 5 then 'L' else '#'
                     x -> x
                 where occupied = length . filter (\x -> grid ! x == '#') $ (sight ! pos)

gridToString grid = intercalate "\n" [[grid ! (i, j) | j <- range (clo, chi)] | i <- range (rlo, rhi)] where
    ((rlo, clo), (rhi, chi)) = bounds grid

process x = do
    let rows = lines x
    let num_rows = length rows
    let num_cols = length (head rows)
    let indexed_rows = join
                     . map (\(i, cols) -> zipWith (\j c -> ((i, j), c)) [1..] cols)
                     . zip [1..] $ rows
    let grid = array ((1, 1), (num_rows, num_cols)) indexed_rows
    let sight = array ((1, 1), (num_rows, num_cols)) (map (\(pos, _) -> (pos, linesOfSight grid pos)) $ indexed_rows)
    let result = converge (evolve sight) grid
    putStrLn . show .length . filter (== '#') . elems $ result

main = readFile filename >>= process
