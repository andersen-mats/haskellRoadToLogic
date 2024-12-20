infix 1 <==>

(<==>) :: Bool -> Bool -> Bool
x <==> y = x == y

infix 1 ==>

(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y

infix 2 <+>

(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y


f1 :: Bool -> Bool -> Bool
f1 p q = (not p) && (p ==> q) <==> not (q && (not p))


f2 :: Bool -> Bool -> Bool
f2 p q = (not p) && (p ==> q) <==> not (q && (not p))


valid1 :: (Bool -> Bool) -> Bool
valid1 bf = (bf True) && (bf False)


valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 bf = bf True True &&
            bf True False &&
            bf False False &&
            bf False True


valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid3 bf = and [ bf p q r | p <- [True, False],
                  q <- [True, False],
                  r <- [True, False]]


valid4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
valid4 bf = and [ bf p q r s |
                  p <- [True, False],
                  q <- [True, False],
                  r <- [True, False],
                  s <- [True, False]]


logEquiv1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bf1 bf2 =
  (bf1 True <==> bf2 True) && (bf1 False <==> bf2 False)


logEquiv2 :: (Bool -> Bool -> Bool) ->
             (Bool -> Bool -> Bool) -> Bool
logEquiv2 bf1 bf2 =
  and [(bf1 p q) <==> (bf2 p q) |
       p <- [True, False],
       q <- [True, False]]


logEquiv3 :: (Bool -> Bool -> Bool -> Bool) ->
             (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 bf1 bf2 = and [(bf1 p q r) <==> (bf2 p q r) |
                     p <- [True, False],
                     q <- [True, False],
                     r <- [True, False]]
