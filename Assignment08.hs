module Assignment08 where 

isInMonth :: (Int, Int, Int) -> [Int] -> Int
isInMonth (y,m,d) x
  | x == m = 1
  | otherwise = 0

numberInMonth :: [(Int, Int, Int)] -> Int -> Int
numberInMonth [] _ = 0
numberInMonth (x:xs) y = isInMonth x y + numberInMonth xs y
