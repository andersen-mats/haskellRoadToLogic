lengths :: [[a]] -> [Integer]
lengths [] = []
lengths (x:xs) = length' x : lengths xs where
  length' :: [a] -> Integer
  length' [] = 0
  length' (x:xs) = length' xs + 1
