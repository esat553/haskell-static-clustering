-- precode
data Expr = Const Int
          | Plus Expr Expr
          | Times Expr Expr
          deriving (Show, Eq)


simplify :: Expr -> Expr
simplify (Times (Const 1) (Const n)) = n
simplify (Const n) = Const n
simplify (Plus e1 e2) = Plus (simplify e1) (simplify e2)

{-
Ground Truth Metadata:
Error Type: Falscher Rückgabetyp / Incorrect Return Type
Short Description: The clause `simplify (Times (Const 1) (Const n)) = n` returns an `Int`, but the function is declared to return an `Expr`, so the result must be wrapped using the `Const` constructor.
Intended Root Cause: Student forgets that pattern-matched constants still need to be returned in the context of the `Expr` type and mistakenly returns a raw `Int`.
Affected Line(s): 9
-}
-- RegEx-Cluster: Typenkonflikt