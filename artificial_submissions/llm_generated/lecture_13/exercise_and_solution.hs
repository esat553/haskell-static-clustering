{-
Implementiere eine Funktion simplify :: Expr -> Expr, die arithmetische Ausdrücke vereinfacht, indem sie Plus (Const 0) e, Plus e (Const 0), Times (Const 1) e und Times e (Const 1) rekursiv zu e reduziert.
-}
{-
english translation:
Implement a function simplify :: Expr -> Expr that simplifies arithmetic expressions by recursively reducing
Plus (Const 0) e, Plus e (Const 0), Times (Const 1) e, and Times e (Const 1) to e.
-}
-- precode
data Expr = Const Int
          | Plus Expr Expr
          | Times Expr Expr
          deriving (Show, Eq)

simplify :: Expr -> Expr
simplify (Plus (Const 0) e) = simplify e
simplify (Plus e (Const 0)) = simplify e
simplify (Plus e1 e2)       = Plus (simplify e1) (simplify e2)
simplify (Times (Const 1) e) = simplify e
simplify (Times e (Const 1)) = simplify e
simplify (Times e1 e2)      = Times (simplify e1) (simplify e2)
simplify (Const n)          = Const n
