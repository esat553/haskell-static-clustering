data ComplexNumber = C (Double, Double)

instance Show ComplexNumber where
    show (C (a, b)) = show a ++ " + " ++ show b ++ "i"

instance Semigroup Complexnumber where
    (C (a1, b1)) <> (C (a2, b2)) =
        C (a1 * a2 - b1 * b2, a1 * b2 + b1 * a2)

instance Monoid ComplexNumber where
    mempty = C (1, 0)

{-
Ground Truth Metadata:
Error Type: Schreibfehler im Typnamen / Spelling Error in Type Name
Short Description: The type name 'Complexnumber' is misspelled (incorrect capitalization) in the 'Semigroup' instance; it should be 'ComplexNumber' to match the data type declaration.
Intended Root Cause: Student makes a typo or is inconsistent with capitalization when referencing a previously defined type.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 6
-}
-- RegEx-Cluster: Typenkonstruktor oder Klasse nicht definiert