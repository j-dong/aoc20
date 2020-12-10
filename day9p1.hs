import Control.Monad
import Control.Arrow
import System.IO
import Data.Maybe
import Data.List
import Data.Char
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Text.Read
import Debug.Trace

filename = "day9_input"

num = 25

prevs l = take (length l - (num + 1))
        . map (initLast . take (num + 1))
        . tails
        $ l
        where initLast l = (init l, last l)

valid (prev, x) = (Just x) `elem` map plus_ne (pairs prev)
    where pairs = (\l' -> map ((,) (head l')) (tail l')) <=< init . tails
          plus_ne (x, y) = if x == y then Nothing else Just (x + y)

process = putStrLn . show
        . snd
        . fromJust
        . find (not . valid)
        . prevs
        . map read
        . lines

main = readFile filename >>= process
