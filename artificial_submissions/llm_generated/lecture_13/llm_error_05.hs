-- precode
data Expr = Const Int
          | Plus Expr Expr
          | Times Expr Expr
          deriving (Show, Eq)

simplify :: Expr -> Expr
simplify (Plus e1 e2) = if e1 == Const 0 then simplify e2
simplify (Times e1 e2) = Times (simplify e1) (simplify e2)
simplify (Const n) = Const n

{-
Ground Truth Metadata:
Error Type: Unvollständiger if-Ausdruck / Incomplete if-Expression
Short Description: The `if` expression in the `Plus` case is missing an `else` branch, which is required in Haskell to make the conditional expression complete.
Intended Root Cause: Student mistakenly assumes that an `if` expression can be used like in imperative languages without requiring an `else` branch.
Affected Line(s): after 8
-}
-- RegEx-Cluster: Parse-Fehler