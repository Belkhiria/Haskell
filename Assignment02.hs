module Assignment02 where
 
sumOfSquares :: [Int] -> Int 
sumOfSquares [] = 0
sumOfSquares xs = (head xs)^2 + sumOfSquares (tail xs)
