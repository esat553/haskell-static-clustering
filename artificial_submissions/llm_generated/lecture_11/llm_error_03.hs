data Box a = Full a | Empty

instance Functor Box where
  fmap f (Full x) = f x
  fmap _ Empty    = Empty

{-
Ground Truth Metadata:
Error Type: Falscher Rückgabetyp in Functor-Instanz / Incorrect Return Type in Functor Instance
Short Description: The function `fmap` returns the bare result `f x`, but the Functor instance for `Box` must return a `Box b`, requiring the result to be wrapped with `Full`.
Intended Root Cause: Student forgets that `fmap` must preserve the container structure and assumes that transforming the inner value is sufficient without reconstructing the outer type.
Affected Line(s): 4
-}
-- RegEx-Cluster: Implementierung verletzt Typsignatur