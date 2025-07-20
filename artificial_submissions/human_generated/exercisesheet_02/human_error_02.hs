rest' :: String -> String
    rest' (_:xs) = xs

{-
Ground Truth Metadata:
Error Type: Einrückungsfehler / Indentation Error
Short Description: The function definition is indented, which is not allowed for top-level declarations in Haskell.
Intended Root Cause: Student is unaware of or overlooks Haskell's requirement for top-level code alignment.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 2
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich