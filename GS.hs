divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k^2 >   n   = n
        | otherwise   = ldf (k+1) n

prime0 :: Integer -> Bool
prime0 n | n < 1 = error "not a negative number"
         | n == 1 = False
         | otherwise = ld n == n
         
average :: [Int] -> Rational
average [] = error "empty list"
average xs = toRational (sum xs) / toRational (length xs)

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + (sum' xs)

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + (length' xs)

mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

mxmInt :: [Int] -> Int
mxmInt [] = error "empty list"
mxmInt [x] = x

mxmInt (x:xs) = max x (mxmInt xs)
removeFst :: Int -> [Int] -> [Int]
removeFst n (x:xs) | n == x = xs
                   | otherwise = x:(removeFst n xs)
                   
reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]

srtsInts :: [Int] -> [Int]
srtsInts [] = []
srtsInts xs = m:(srtsInts (removeFst m xs)) where m = mnmInt xs

min' :: Int -> Int -> Int
min' x y | x <= y = x
         | otherwise = y

srtsInts' :: [Int] -> [Int]
srtsInts' [] = []
srtsInts' xs = let
  m = mnmInt xs
  in m:(srtsInts' (removeFst m xs))

neg False = True
neg True = False


map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map f xs
