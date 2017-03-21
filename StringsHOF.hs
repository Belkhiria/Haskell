module StringsHOF where

import Data.List
import Data.Char

onlyCapitals :: [[Char]] -> [[Char]]
onlyCapitals [] = []
onlyCapitals (x:xs) = headd ++ tailss
  where headd = if head (x) `elem` ['A'..'Z'] then [x] else []
        tailss = onlyCapitals xs

helperFunction :: (Int -> Int -> Bool) -> [[Char]] -> [Char]
helperFunction f y = foldr (\x acc -> if f (length acc) (length x) then acc else x ) [] y 

longestString1 :: [[Char]] -> [Char]
longestString1 x = helperFunction (>) x 

longestString2 :: [[Char]] -> [Char]
longestString2 x = helperFunction (>=) x


longestCapitalized :: [[Char]] -> [Char]
longestCapitalized [] = []
longestCapitalized (x:xs) = longestString1 ( onlyCapitals (x:xs) )

