class TF p where
  valid :: p -> Bool
  lequiv :: p -> p -> Bool

instance TF Bool
  where
    valid = id
    lequiv f g = f == g

instance TF p => TF (Bool -> p)
  where
    valid f = valid (f True) && valid (f False)
    lequiv f g = (f True) `lequiv` (g True)
                 && (f False) `lequiv` (g False)
