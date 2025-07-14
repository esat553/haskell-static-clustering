plus :: Integer -> Integer -> Integer
plus _ y = y
plus x _ = x
plus f x y = f x + f y

minus :: Integer -> Integer -> Integer
minus _ y = y
minus x _ = error "empty"
minus x y = y - x

{-
error:
    Equations for ‘plus’ have different numbers of arguments
      Studentenlösung:2:1-12
      Studentenlösung:4:1-22
  |
2 | plus _ y = y
  | ^^^^^^^^^^^^^...
-}