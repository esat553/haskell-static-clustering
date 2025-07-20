data Box a
  = Full a
  = Empty

instance Functor Box where
  fmap f (Full x) = Full (f x)
  fmap _ Empty    = Empty

{-
Ground Truth Metadata:
Error Type: Ungültige Datentypdefinition / Invalid Data Type Definition
Short Description: The data declaration uses two `=` symbols to define multiple constructors, but Haskell requires `|` to separate constructors in algebraic data types.
Intended Root Cause: Student confuses the syntax for defining alternative constructors and mistakenly uses `=` instead of `|`.
Affected Line(s): 2–3
-}
-- RegEx-Cluster: Parse-Fehler