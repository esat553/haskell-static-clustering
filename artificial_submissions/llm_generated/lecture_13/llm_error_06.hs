-- precode
data Expr = Const Int
          | Plus Expr Expr
          | Times Expr Expr
          deriving (Show, Eq)


simplify :: Expr -> Expr
simplify (Expr n) = Expr n
simplify (Plus (Const 0) e) = simplify e
simplify (Times (Const 1) e) = simplify e

{-
Ground Truth Metadata:
Error Type: Ungültiges Pattern-Matching auf Typnamen / Invalid Pattern Match on Type Name
Short Description: The pattern `(Expr n)` uses the type name `Expr` as if it were a data constructor, but `Expr` is a type, not a constructor. Only the constructors `Const`, `Plus`, and `Times` are valid for pattern matching.
Intended Root Cause: Student confuses the type `Expr` with its data constructors and attempts to match on the type name instead of a valid constructor.
Affected Line(s): 7
-}
-- RegEx-Cluster: Nicht definierter Datenkonstruktor