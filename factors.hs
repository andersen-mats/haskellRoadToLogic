divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k^2 >   n   = n
        | otherwise   = ldf (k+1) n

factors :: Integer -> [Integer]
factors n | n < 1 = error "argument not positive"
          | n == 1 = []
          | otherwise = p : factors (div n p) where p = ld n
