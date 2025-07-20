foo = sqrt . 5 * . sum . flip take [1..50]

{-
Ground Truth Metadata:
Error Type: Fehlende Klammerung im Ausdruck / Missing Parentheses in Expression
Short Description: Parentheses are missing around the partial application '(5 *)', leading to a syntax or type error in the function composition.
Intended Root Cause: Student is not aware that operators like '*' require parentheses when partially applied in pointfree expressions.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Parse-Fehler