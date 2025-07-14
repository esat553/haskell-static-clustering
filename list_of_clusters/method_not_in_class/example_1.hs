data BB a = L | K a (BB a) (BB a)

instance Eq a => BB a where
  (==) t1 t2 = True

  {-
  Studentenlösung:4:3-6: error:
    ‘==’ is not a (visible) method of class ‘BB’
  |
4 |   (==) t1 t2 = True
  |   ^^^^
  -}