data ComplexNumber = C (Double, Double)

instance show ComplexNumber where
    show (C (a, b)) = show a ++ " + " ++ show b ++ "i"

instance semigroup ComplexNumber where
    (C (a1, b1)) <> (C (a2, b2)) =
        C (a1 * a2 - b1 * b2, a1 * b2 + b1 * a2)

instance monoid ComplexNumber where
    mempty = C (1, 0)

{-
Ground Truth Metadata:
Error Type: Falsche Groß-/Kleinschreibung in Typdefinition / Incorrect Capitalization in Type Declaration
Short Description: The type class names 'show', 'semigroup', and 'monoid' are written in lowercase instead of the required capitalized forms 'Show', 'Semigroup', and 'Monoid'.
Intended Root Cause: Student is unaware that type class names must be capitalized in Haskell, or confuses them with method names which are usually lowercase.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 3, 6, 10
-}
-- RegEx-Cluster: Methode nicht in Klasse