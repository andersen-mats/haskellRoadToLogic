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

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) | p x       = x : filter' p xs
                 | otherwise = filter' p xs

primes0 :: [Integer] -> [Integer]
primes0 xs = filter' prime0 xs
