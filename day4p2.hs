import Control.Monad
import Control.Arrow
import System.IO
import Data.Maybe
import Data.List
import Data.Char
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

fieldsPresent :: [Entry] -> Bool
fieldsPresent = Set.isSubsetOf (Set.fromList requiredFields)
              . Set.fromList
              . map fst

isNDigits :: Int -> String -> Bool
isNDigits n s = (length s == n) && all isDigit s

inRange :: Ord a => (a, a) -> a -> Bool
inRange (lo, hi) x = (x >= lo) && (x <= hi)

checkHeight :: String -> Maybe ()
checkHeight x = return (reverse x)
              >>= checkHeight'
              >>= checkHeight''
                where checkHeight'' (range, s) = if all isDigit s && inRange range (read (reverse s)) then Just () else Nothing
                      checkHeight' ('n':'i':xs) = Just ((59, 76), xs)
                      checkHeight' ('m':'c':xs) = Just ((150, 193), xs)
                      checkHeight' _ = Nothing

fieldValid :: Entry -> Bool
fieldValid ("byr", x) = isNDigits 4 x && inRange (1920, 2002) (read x)
fieldValid ("iyr", x) = isNDigits 4 x && inRange (2010, 2020) (read x)
fieldValid ("eyr", x) = isNDigits 4 x && inRange (2020, 2030) (read x)
fieldValid ("hgt", x) = isJust (checkHeight x)
fieldValid ("hcl", x) = (length x == 7) && (head x == '#') && all isHexDigit (tail x)
fieldValid ("ecl", "amb") = True
fieldValid ("ecl", "blu") = True
fieldValid ("ecl", "brn") = True
fieldValid ("ecl", "gry") = True
fieldValid ("ecl", "grn") = True
fieldValid ("ecl", "hzl") = True
fieldValid ("ecl", "oth") = True
fieldValid ("ecl", _) = False
fieldValid ("pid", x) = isNDigits 9 x
fieldValid _ = True

passportValid :: [Entry] -> Bool
passportValid pass = fieldsPresent pass && all fieldValid pass

process = putStrLn . show . length
        . filter passportValid
        . map parsePassport
        . split ""
        . lines

main = readFile filename >>= process
