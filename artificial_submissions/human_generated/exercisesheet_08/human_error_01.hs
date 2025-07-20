data complexNumber = C (Double, Double)

instance Show complexNumber where
    show (C (a, b)) = show a ++ " + " ++ show b ++ "i"

instance Semigroup complexNumber where
    (C (a1, b1)) <> (C (a2, b2)) =
        C (a1 * a2 - b1 * b2, a1 * b2 + b1 * a2)

instance Monoid complexNumber where
    mempty = C (1, 0)

{-
Ground Truth Metadata:
Error Type: Falsche Groß-/Kleinschreibung beim Typkonstruktor / Incorrect Capitalization of Type Constructor
Short Description: The data type 'complexNumber' is written in lowercase; in Haskell, type and constructor names must start with an uppercase letter.
Intended Root Cause: Student is unaware of or forgets Haskell's naming convention that requires type constructors to begin with a capital letter.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Fehlerhafter Typ-Header