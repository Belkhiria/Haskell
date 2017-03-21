module Assignment05 where 

reverse2' :: [a] -> [a] -> [a]
reverse2' [] list = list
reverse2' (x:xs) list = reverse2' xs (x : list) 

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse2' (x:xs) [] 
