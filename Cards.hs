module Cards where

type Card = (Suit, Rank)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Eq)
data Rank = Jack | Queen | King | Ace | Num Int deriving (Show, Eq)
data Color = Red | Black deriving (Show, Eq)

data IllegalMove = IllegalMove deriving (Show, Eq)

cardColor :: Card -> Color
cardColor (Clubs,_) = Black
cardColor (Spades,_) = Black
cardColor (Diamonds,_) = Red
cardColor (Hearts,_) = Red
 

cardValue :: Card -> Int
cardValue (_,Num x) = x
cardValue (_,rank)
 | rank == Ace = 11
 | otherwise = 10

removeCard :: [Card] -> Card -> Either IllegalMove [Card]
removeCard [] _ = Left IllegalMove
removeCard (x:xs) card
 | not ( card `elem` (x:xs) ) = Left IllegalMove
 | card == x = Right xs
 | otherwise = Right ([x] ++ either (const []) id ( removeCard xs card ))

allSameColor :: [Card] -> Bool
allSameColor [] = True
allSameColor [card] = True
allSameColor (x:xs) = cardColor x == cardColor (head(xs)) && allSameColor xs

tailRecursion :: [Card] -> Int -> Int
tailRecursion [card] count = count + cardValue card
tailRecursion (x:xs) count = tailRecursion xs count + cardValue x

sumCards :: [Card] -> Int
sumCards [] = 0
sumCards (x:xs) = tailRecursion (x:xs) 0

score :: [Card] -> Int -> Int
score [] x = x `div` 2
score (x:xs) s
 | allSameColor (x:xs) == True = temporaryScore `div` 2
 | otherwise = temporaryScore
 where sumCardValue = sumCards (x:xs)
       temporaryScore = if sumCardValue > s then (sumCardValue - s)*3 else (s - sumCardValue)

--inRight (Right z) = z

data Move = Discard Card | Draw deriving (Show, Eq)

recursiveInnerHelper :: [Card] -> [Move] ->  Int -> [Card] -> Either IllegalMove Int
recursiveInnerHelper [] (Discard x : _) goal cards = Left IllegalMove
recursiveInnerHelper [] (Draw : _) goal cards = Right ( (score cards goal) )
recursiveInnerHelper _ [] goal cards = Right ( score cards goal )
recursiveInnerHelper (x:xs) (y:ys) goal cards = case y of
                                                Discard card  -> if ( card `elem` cards ) then recursiveInnerHelper (x:xs) ys goal (inRight(removeCard cards card)) else Left IllegalMove
                                                Draw -> if ((sumCards (x:cards)) < goal) then recursiveInnerHelper xs ys goal (x:cards) else Right ( score (x:cards) goal)
                                                where inRight (Right z) = z



officiate :: [Card] -> [Move] -> Int -> Either IllegalMove Int
officiate cards moves goal = recursiveInnerHelper cards moves goal []
