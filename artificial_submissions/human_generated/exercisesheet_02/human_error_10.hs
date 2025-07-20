rest :: String -> String
rest x = drop 1 xs

{-
Ground Truth Metadata:
Error Type: Verwechslung der Variablen / Variable Mix-up
Short Description: The function uses 'xs' in the implementation, but the argument declared is 'x'; 'xs' is undefined.
Intended Root Cause: Student mixes up variable names, using a variable that is not bound in the function scope.
Error Class: Mental Typo (Korkmaz et al. 2015, Level 0)
Affected Line(s): 2
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich