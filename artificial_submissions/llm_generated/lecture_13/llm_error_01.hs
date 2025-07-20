-- precode
data Expr = Const Int
          | Plus Expr Expr
          | Times Expr Expr
          deriving (Show, Eq)

simplify :: Expr -> Expr
simplify (Plus 0 e) = simplify e
simplify (Plus e 0) = simplify e
simplify (Times 1 e) = simplify e
simplify (Times e 1) = simplify e
simplify (Const n) = Const n
simplify (Plus e1 e2) = Plus (simplify e1) (simplify e2)
simplify (Times e1 e2) = Times (simplify e1) (simplify e2)

{-
Ground Truth Metadata:
Error Type: Falscher Pattern-Match auf Konstruktorargumente / Invalid Pattern Matching on Constructor Arguments
Short Description: The patterns `Plus 0 e` and `Times e 1` attempt to match `Int` values directly, but in the `Expr` datatype, constants are wrapped in `Const`, so the correct match would be `Plus (Const 0) e`, not `Plus 0 e`.
Intended Root Cause: Student overlooks that all numeric constants in the `Expr` type are wrapped using the `Const` constructor and mistakenly matches on raw `Int` values instead.
Affected Line(s): 8-11
-}
-- RegEx-Cluster: Numerischer Typenkonflikt