import Control.Monad
import Control.Arrow
import System.IO
import Data.Maybe
import Data.List
import qualified Data.Set as Set
import Text.Read

filename = "day4_input"

type Entry = (String, String)

requiredFields = [ "byr"
                 , "iyr"
                 , "eyr"
                 , "hgt"
                 , "hcl"
                 , "ecl"
                 , "pid"
                 -- , "cid"
                 ]

splitP :: (a -> Bool) -> [a] -> [[a]]
splitP _ [] = []
splitP p xs = group : (splitP p (dropWhile p rest))
             where (group, rest) = break p xs

split :: Eq a => a -> [a] -> [[a]]
split d = splitP (== d)

split1 :: Eq a => a -> [a] -> Maybe ([a], [a])
split1 d = handle . break (== d)
           where handle (_, []) = Nothing
                 handle (a, (_:b)) = Just (a, b)

parsePassport :: [String] -> [Entry]
parsePassport = catMaybes . map (split1 ':') . (>>= (split ' '))

passportValid :: [Entry] -> Bool
passportValid = Set.isSubsetOf (Set.fromList requiredFields)
              . Set.fromList
              . map fst

process = putStrLn . show . length
        . filter passportValid
        . map parsePassport
        . split ""
        . lines

main = readFile filename >>= process
