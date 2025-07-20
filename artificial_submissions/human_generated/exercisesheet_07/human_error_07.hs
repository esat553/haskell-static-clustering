foo :: Int -> Doubled
foo = sqrt . (5 *) . sum . flip take [1..50]

{-
Ground Truth Metadata:
Error Type: Schreibfehler im Typnamen / Spelling Error in Type Name
Short Description: The type name 'Doubled' is misspelled; it should be 'Double' in the function type declaration.
Intended Root Cause: Student makes a typographical error in the type name, likely due to inattention or unfamiliarity with standard Haskell types.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Typenkonstruktor oder Klasse nicht definiert