data ExDaTy = ExDaTy Int Int deriving Show

instance Show ExDaTy where
    show (ExDaTy a b) = show(a) ++ show(b)

{-
Studentenlösung:1:39-42: error:
    Duplicate instance declarations:
      instance Show ExDaTy -- Defined at Studentenlösung:1:39
      instance Show ExDaTy -- Defined at Studentenlösung:3:10
  |
1 | data ExDaTy = ExDaTy Int Int deriving Show
  |                                       ^^^^
-}