data Box a = Full a | Empty

instance Functor Box where
  fmap f (Full x) = Full (f x)
  fmap _          = Empty

{-
Ground Truth Metadata:
Error Type: Inkonsistente Funktionsdefinition / Inconsistent Function Definition Arity
Short Description: The second clause of `fmap` omits the argument pattern `(Empty)`, resulting in a function definition with inconsistent arity. Haskell requires all pattern matches in a function definition to have the same number of arguments.
Intended Root Cause: Student attempts to simplify the pattern match for `Empty` by omitting the argument entirely, not realizing that this breaks the structural consistency of the function definition.
Affected Line(s): 5
-}
-- RegEx-Cluster: Abweichende Arity