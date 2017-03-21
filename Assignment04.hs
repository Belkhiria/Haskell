module Assignment04 where 

elementAt :: [a] -> Int -> a
elementAt (x:xs) i
  | c == i = x
  | otherwise = elementAt xs (i-1)
  where c = 0

