data ComplexNumber = C (Double, Double)

instance Show ComplexNumber where
    show (C (a, b)) = show a ++ " + " ++ show b ++ "i"

instance Semigroup ComplexNumber where
    (C (a1, b1)) <> (C (a2, b2)) =
        C (a1 * a2 - b1 * b2, a1 * b2 + b1 * a2)

instance Monoid ComplexNumber where
    mempty = 0.0

{-
Ground Truth Metadata:
Error Type: Typkonflikt / Type Mismatch
Short Description: The value assigned to 'mempty' is '0.0', which is of type 'Double' instead of the required type 'ComplexNumber'.
Intended Root Cause: Student confuses the expected type of 'mempty' in the 'Monoid' instance and provides a value of the wrong type.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 12
-}
-- RegEx-Cluster: Numerischer Typenkonflikt