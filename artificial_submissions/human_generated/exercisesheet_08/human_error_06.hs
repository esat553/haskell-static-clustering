data ComplexNumber = C (Double, Double)

instance Show ComplexNumber where
    show (C (a, b)) = show a ++ " + " ++ show b ++ "i"

instance Semigroup ComplexNumber where
    (C (a1, b1)) ++ (C (a2, b2)) =
        C (a1 * a2 - b1 * b2, a1 * b2 + b1 * a2)

instance Monoid ComplexNumber where
    mempty = C (1, 0)

{-
Ground Truth Metadata:
Error Type: Falsche Operatorwahl / Incorrect Operator Choice
Short Description: The list concatenation operator '++' is used instead of the semigroup operator '<>' for combining values in the 'Semigroup' instance.
Intended Root Cause: Student confuses standard list operators with those required by the Semigroup type class, possibly due to habit or lack of familiarity with type class operator conventions.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 7
-}
-- RegEx-Cluster: Methode nicht in Klasse