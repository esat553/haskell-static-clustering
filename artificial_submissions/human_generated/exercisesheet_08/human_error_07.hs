data ComplexNumber = C (Double, Double)

instance Show ComplexNumber where
    show (C (a b)) = show a ++ " + " ++ show b ++ "i"

instance Semigroup ComplexNumber where
    (C (a1, b1)) <> (C (a2, b2)) =
        C (a1 * a2 - b1 * b2, a1 * b2 + b1 * a2)

instance Monoid ComplexNumber where
    mempty = C (1, 0)

{-
Ground Truth Metadata:
Error Type: Syntaxfehler durch fehlendes Trennzeichen / Syntax Error Due to Missing Separator
Short Description: The comma is missing between 'a' and 'b' in the pattern '(a b)' instead of '(a, b)', causing a syntax error in the pattern match.
Intended Root Cause: Student forgets that tuple elements must be separated by a comma in Haskell pattern matching.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 4
-}
-- RegEx-Cluster: Parse-Fehler