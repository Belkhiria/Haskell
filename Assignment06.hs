module Assignment06 where 

isOlder :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
isOlder (x1,y1,z1) (x2,y2,z2)
  | x1 < x2 = True
  | x1 == x2 && y1 < y2 = True
  | x1 == x2 && y2 == y1 && z1 < z2 = True
  | otherwise = False
