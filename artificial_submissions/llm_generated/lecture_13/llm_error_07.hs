-- precode
data Expr = Const Int
          | Plus Expr Expr
          | Times Expr Expr
          deriving (Show, Eq)


simplify :: Expr -> Int
simplify (Plus (Const 0) e) = simplify e
simplify (Plus e (Const 0)) = simplify e
simplify (Const n) = Const n
simplify (Plus e1 e2) = Plus (simplify e1) (simplify e2)

{-
Ground Truth Metadata:
Error Type: Typinkonsistenz zwischen Signatur und Rückgabewert / Type Inconsistency Between Signature and Return Value
Short Description: The type signature declares a return type of `Int`, but the function returns values of type `Expr`, such as `Const n` or `Plus (...) (...)`.
Intended Root Cause: Student specifies an incorrect return type in the function signature, likely assuming the simplification should produce a numeric result rather than a simplified expression.
Affected Line(s): 11-12
-}
-- RegEx-Cluster: Typenkonflikt