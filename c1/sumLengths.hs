sumLengths :: [[a]] -> Integer
sumLengths [] = 0
sumLengths (x:xs) = length' x + sumLengths xs where
  length' :: [a] -> Integer
  length' [] = 0
  length' (y:ys) = length' ys + 1
  
