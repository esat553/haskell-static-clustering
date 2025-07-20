-- precode
data Expr = Const Int
          | Plus Expr Expr
          | Times Expr Expr
          deriving (Show, Eq)


simplify :: Expr -> Expr
simplify (Plus (Const 0) e) = simplify e
simplify (Plus e (Const 0)) = simplify e
simplify (Times (Const 1) e) = simplify e
simplify (Times e (Const 1)) = simplify e
simplify (Const n) = Const n
simplify (Plus e1 e2) = simplify e2
simplify (Times e1 e2) = simplify e1

{- no compile error found -}