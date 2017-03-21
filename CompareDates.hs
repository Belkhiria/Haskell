module CompareDates where 
import Data.Maybe
isInMonth :: (Int, Int, Int) -> Int -> Int
isInMonth (y,m,d) x
  | x == m = 1
  | otherwise = 0

numberInMonth :: [(Int, Int, Int)] -> Int -> Int
numberInMonth [] _ = 0
numberInMonth (x:xs) y = isInMonth x y + numberInMonth xs y


numberInMonths :: [(Int, Int, Int)] -> [Int] -> Int
numberInMonths [] _ = 0
numberInMonths _ [] = 0
numberInMonths (x:xs)(y:ys) = numberInMonth (x:xs) y + numberInMonths (x:xs) ys

type Date = (Int, Int, Int)

isInMonth2 :: Date -> Int -> Int
isInMonth2 (y,m,d) x
  | x == m = 1
  | otherwise = 0


datesInMonth :: [Date] -> Int -> [Date]
datesInMonth (x:xs) 0 = []
datesInMonth [] _ = []
datesInMonth (x:xs) y 
   | isInMonth2 x y == 1 = [x] ++ datesInMonth xs y
   | otherwise = [] ++ datesInMonth xs y

isOlder :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
isOlder (x1,y1,z1) (x2,y2,z2)
  | x1 < x2 = True
  | x1 == x2 && y1 < y2 = True
  | x1 == x2 && y2 == y1 && z1 < z2 = True
  | otherwise = False


oldestDate :: [(Int, Int, Int)] -> Maybe (Int, Int, Int)
oldestDate [] = Nothing
oldestDate [x] = Just x
oldestDate (x:xs)
 | Just x < max = Just x
 | otherwise =  max
 where max = oldestDate xs
