module AllExcept where 
import Data.Maybe

allExcept :: [Char] -> [[Char]] -> Maybe [[Char]]
allExcept _ [[]] = Nothing
allExcept [] _ = Nothing
allExcept st (x:xs)
  | not (st `elem` (x:xs)) = Nothing
  | st == x = Just xs
  | otherwise = Just ( [x] ++ fromJust (allExcept st xs ) ) 
