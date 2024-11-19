count :: Char -> String -> Int
count c [] = 0
count c (t:ts) | c == t = 1 + (count c ts)
               | otherwise = 0 + (count c ts)
