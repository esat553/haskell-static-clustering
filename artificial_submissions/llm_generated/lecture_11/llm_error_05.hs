data Box a = Full a | Empty

instance Foldable Box where
  foldMap :: (a -> m) -> Box a -> m
  foldMap f (Full x) = f x
  foldMap _ Empty    = mempty

{-
Ground Truth Metadata:
Error Type: Fehlende Typklassen-Constraint / Missing Type Class Constraint
Short Description: The type signature of `foldMap` omits the required `Monoid m =>` constraint, which is necessary for using `mempty` in the implementation.
Intended Root Cause: Student forgets to include the required constraint for monoidal operations and assumes `mempty` can be used without declaring that `m` is a `Monoid`.
Affected Line(s): 3
-}
-- RegEx-Cluster: Implementierung verletzt Typsignatur