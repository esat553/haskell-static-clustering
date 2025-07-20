rest' :: String -> String
rest' (_:xs) = x

{-
Ground Truth Metadata:
Error Type: Verwechslung der Variablen / Variable Mix-up
Short Description: The function returns 'x', but only 'xs' is bound in the pattern; 'x' is not defined in this scope.
Intended Root Cause: Student mixes up variable names, possibly due to carelessness or misunderstanding of pattern matching.
Error Class: Mental Typo (Korkmaz et al. 2015, Level 0)
Affected Line(s): 2
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich