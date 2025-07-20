data ComplexNumber = C (Double, Double)

instance Show ComplexNumber where
    show (C (a, b)) = show a ++ " + " ++ show b ++ "i"


instance Monoid ComplexNumber where
    mempty = C (1, 0)

{-
Ground Truth Metadata:
Error Type: Fehlende Instanziierung / Missing Instance Declaration
Short Description: The 'Semigroup' instance for 'ComplexNumber' is missing, but it is required for the 'Monoid' instance to be valid.
Intended Root Cause: Student does not know or has overlooked that 'Monoid' requires an explicit 'Semigroup' instance in recent Haskell versions.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): missing
-}
-- RegEx-Cluster: Fehlende Superklassen-Instanz