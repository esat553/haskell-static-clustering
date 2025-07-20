-- precode
data Expr = Const Int
          | Plus Expr Expr
          | Times Expr Expr
          deriving (Show, Eq)


simplify :: Expr -> Expr
simplify expr = do
  let simplifiedExpr = simplify expr
  return simplifiedExpr

{-
Ground Truth Metadata:
Error Type: Ungültige Verwendung der do-Notation / Invalid Use of do-Notation
Short Description: The function `simplify` uses `do`-notation, which is only valid in monadic contexts (like `IO`, `Maybe`, etc.), but `Expr` is not a monad.
Intended Root Cause: Student attempts to write imperative-style code in a pure function and misapplies monadic syntax where standard expression evaluation is required.
Affected Line(s): 8–11
-}
-- RegEx-Cluster: Typenkonflikt