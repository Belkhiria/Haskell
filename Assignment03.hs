module Assignment03 where

mappedSum :: (Int -> Int) -> [Int] -> Int
mappedSum _ [] = 0
mappedSum f (x:xs) = f x + mappedSum f xs

