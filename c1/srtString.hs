getChars :: String -> [Int]
getChars = map fromEnum

least :: [String] -> String
least [s] = s
least (s:st) = if getChars s <= getChars m then s else m
  where m = least st

rmvFst :: String -> [String] -> [String]
rmvFst n (x:xs) | n == x = xs
                | otherwise = x : (rmvFst n xs)

srtString :: [String] -> [String]
srtString [] = []
srtString xs = m : (srtString (rmvFst m xs))
  where m = least xs
