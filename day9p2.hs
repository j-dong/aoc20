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

attach f g l = zip (map f l) (map g l)

process x = do
    let nums = map read . lines $ x :: [Int]
    let target = snd . fromJust . find (not . valid) . prevs $ nums
    let summed_tails = map (first getSum) . scanr mappend mempty . (attach Sum return) :: [Int] -> [(Int, [Int])]
    let sum_subarrs = inits nums >>= summed_tails
    -- TODO: optimize
    let weakness' = snd . fromJust . find (\(s, l) -> length l >= 2 && s == target) $ sum_subarrs
    putStrLn . show $ (minimum weakness' + maximum weakness')

main = readFile filename >>= process
