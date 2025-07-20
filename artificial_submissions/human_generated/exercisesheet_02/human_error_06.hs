rest :: String -> String -> String
rest xs = drop 1 xs

{-
Ground Truth Metadata:
Error Type: Arity-Fehler / Arity Mismatch
Short Description: The type signature declares a function of two arguments, but the implementation defines it with only one argument.
Intended Root Cause: Student incorrectly writes the type signature, adding an extra argument that is not present in the function definition.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Falsche Funktionsarität