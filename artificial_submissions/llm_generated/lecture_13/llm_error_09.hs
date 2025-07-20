-- precode
data Expr = Const Int
          | Plus Expr Expr
          | Times Expr Expr
          deriving (Show, Eq)


simplify :: Expr -> Expr
simplify expr = case expr
    Plus (Const 0) e -> simplify e
    Plus e (Const 0) -> simplify e
    Times (Const 1) e -> simplify e
    Times e (Const 1) -> simplify e
    Const n -> Const n
    Plus e1 e2 -> Plus (simplify e1) (simplify e2)
    Times e1 e2 -> Times (simplify e1) (simplify e2)

{-
Ground Truth Metadata:
Error Type: Fehlendes 'of' im case-Ausdruck / Missing 'of' in case Expression
Short Description: The `case` expression is missing the `of` keyword, which is required to separate the expression being matched from its pattern clauses in Haskell.
Intended Root Cause: Student forgets the correct syntax of a `case` expression and omits the mandatory `of`, leading to a syntax error.
Affected Line(s): 6
-}
-- RegEx-Cluster: Parse-Fehler