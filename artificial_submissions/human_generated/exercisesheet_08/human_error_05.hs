data ComplexNumber = C (Double, Double)

instance Show ComplexNumber where
    Show (C (a, b)) = show a ++ " + " ++ show b ++ "i"

instance Semigroup ComplexNumber where
    (C (a1, b1)) <> (C (a2, b2)) =
        C (a1 * a2 - b1 * b2, a1 * b2 + b1 * a2)

instance Monoid ComplexNumber where
    mempty = C (1, 0)

{-
Ground Truth Metadata:
Error Type: Falsche Groß-/Kleinschreibung im Funktionsnamen / Incorrect Capitalization in Function Name
Short Description: The method name 'Show' is capitalized instead of being written in lowercase as 'show' in the 'Show' type class instance.
Intended Root Cause: Student confuses the capitalization rules for type class names (which are capitalized) and method names (which are lowercase) in Haskell.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 4
-}
-- RegEx-Cluster: Pattern Binding in Instanz