data Box a = Full a | Empty

instance Functor Box where
  fmap f (Full x) = Full (f x)
  fmap f Empty    = f Empty

{-
Ground Truth Metadata:
Error Type: Falsche Anwendung der Funktion auf einen Konstruktor / Incorrect Function Application to Constructor
Short Description: The expression `f Empty` attempts to apply a function `f :: a -> b` to the constructor `Empty`, which is not a value of type `a` and therefore not a valid argument.
Intended Root Cause: Student mistakenly assumes that the function `f` can be applied to all cases, including constructors without contained values, and overlooks the need to preserve the structure in non-full cases.
Affected Line(s): 5
-}
-- RegEx-Cluster: Implementierung verletzt Typsignatur