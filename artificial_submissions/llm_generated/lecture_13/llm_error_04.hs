-- precode
data Expr = Const Int
          | Plus Expr Expr
          | Times Expr Expr
          deriving (Show, Eq)


simplify :: Expr -> Expr
simplify (Plus (Const 0) e) = simplify e
simplify (Times (Const 1) e) = simplify e
simplify (Plus e1 e2) = simplify e1 + simplify e2
simplify (Times e1 e2) = simplify e1 * simplify e2
simplify (Const n) = Const n

{-
Ground Truth Metadata:
Error Type: Ungültige Verwendung von arithmetischen Operatoren / Invalid Use of Arithmetic Operators
Short Description: The operators `+` and `*` are used directly on values of type `Expr`, but these operators are only defined for numeric types like `Int`, not for the custom type `Expr`.
Intended Root Cause: Student mistakenly assumes that simplified expressions can be directly combined using standard arithmetic operators instead of reconstructing them using the `Plus` and `Times` constructors.
Affected Line(s): 11-12
-}
-- RegEx-Cluster: Numerischer Typenkonflikt