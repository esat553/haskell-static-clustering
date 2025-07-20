data Box a = Full a | Empty

instance Foldable Box where
  foldMap :: Monoid m => (a -> m) -> Box a -> m
  foldMap f (Full x) = x
  foldMap _ Empty    = mempty

{-
Ground Truth Metadata:
Error Type: Fehlende Funktionsanwendung im Fold / Missing Function Application in Fold
Short Description: The implementation of `foldMap` returns `x` directly instead of applying the function `f`, which violates the intended behavior of `foldMap`.
Intended Root Cause: Student misunderstands the role of the function parameter in `foldMap` and forgets to map the content of the structure before folding.
Affected Line(s): 5
-}
-- RegEx-Cluster: Implementierung verletzt Typsignatur