-- precode
data Expr = Const Int
          | Plus Expr Expr
          | Times Expr Expr
          deriving (Show, Eq)


simplify :: Expr -> Expr
simplify (Plus (Const 0) e) -> simplify e
simplify (Plus e (Const 0)) -> simplify e
simplify (Times (Const 1) e) -> simplify e
simplify (Times e (Const 1)) -> simplify e
simplify (Const n)          -> Const n
simplify (Plus e1 e2)       -> Plus (simplify e1) (simplify e2)
simplify (Times e1 e2)      -> Times (simplify e1) (simplify e2)

{-
Ground Truth Metadata:
Error Type: Falsche Syntax für Funktionsdefinition / Incorrect Function Definition Syntax
Short Description: The symbol `->` is incorrectly used in function equations instead of the correct symbol `=`, which is required to define the right-hand side of a pattern match in Haskell.
Intended Root Cause: Student confuses the syntax of case expressions or type signatures (where `->` is valid) with the syntax of function definitions, where `=` must be used.
Affected Line(s): 3–9
-}
-- RegEx-Cluster: Parse-Fehler