data ComplexNumber = C (Double, Double)

instance Show ComplexNumber where
    show (C (a, b)) = show a ++ " + " ++ show b ++ "i"

instance Semigroup ComplexNumber
    (C (a1, b1)) <> (C (a2, b2)) =
        C (a1 * a2 - b1 * b2, a1 * b2 + b1 * a2)

instance Monoid ComplexNumber where
    mempty = C (1, 0)


{-
Ground Truth Metadata:
Error Type: Fehlendes Schlüsselwort / Missing Keyword
Short Description: The keyword 'where' is missing in the instance declaration for 'Semigroup ComplexNumber', resulting in an invalid instance definition.
Intended Root Cause: Student forgets the required 'where' keyword in type class instance definitions, misunderstanding Haskell's syntax rules.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 7
-}
-- RegEx-Cluster: Parse-Fehler