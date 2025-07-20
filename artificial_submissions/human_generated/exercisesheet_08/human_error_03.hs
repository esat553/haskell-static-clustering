data ComplexNumber = C (Double, Double)

instance Show ComplexNumber where
    show (C (a, b)) = show a ++ " + " ++ show b ++ "i"

instance Semigroup ComplexNumber where
    (C (a1, b1)) <> (C (a2, b2)) =
        C (a1 * a2 - b1 * b2, a1 * b2 + b1 * a2)

instance Monoid ComplexNumber where
    mempy = C (1, 0)

{-
Ground Truth Metadata:
Error Type: Schreibfehler bei Typklassenmethode / Spelling Error in Typeclass Method Name
Short Description: The method name 'mempy' is misspelled instead of 'mempty' in the 'Monoid' instance.
Intended Root Cause: Student makes a typographical error in the required method name for a standard type class, indicating a lack of attention to detail.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 11
-}
-- RegEx-Cluster: Methode nicht in Klasse