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

filename = "day10_input"

doPart1 l = numX 1 * numX 3 where
    numX x = length . filter ((== -x) . uncurry (-)) $ l

process = putStrLn . show
        . doPart1
        . (\x -> zip x (tail x))
        . (0 :)
        . (\l -> l ++ [3 + last l])
        . sort
        . map read
        . lines

main = readFile filename >>= process
