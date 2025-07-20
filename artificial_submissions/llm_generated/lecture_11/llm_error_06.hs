data Box a = Full a | Empty

instance Foldable Box where
  foldMap :: Monoid m => (a -> m) -> Box a -> m
  foldMap f (Full x) = f x
  foldMap _ Empty    = 0

{-
Ground Truth Metadata:
Error Type: Falsches neutrales Element für Monoid / Incorrect Neutral Element for Monoid
Short Description: The expression `0` is used as the neutral element, but `foldMap` must return `mempty`, which depends on the specific Monoid instance and is not always `0`.
Intended Root Cause: Student assumes that `0` is the universal identity element for all Monoids, overlooking that different Monoids have different `mempty` values.
Affected Line(s): 6
-}
-- RegEx-Cluster: Constraint nicht erfüllbar