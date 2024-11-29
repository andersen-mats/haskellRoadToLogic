blowup :: String -> String
blowup [] = []
blowup (t:ts) = let
  blowup' n [x] = take n [x,x..]
  blowup' n (x:xs) = (blowup' n [x]) ++ blowup' (n+1) xs
  in blowup' 1 (t:ts)
