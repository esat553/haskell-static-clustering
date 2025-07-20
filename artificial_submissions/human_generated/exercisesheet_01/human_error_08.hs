doubleList :: [Int] -> [Int]
doubleList = maps (*2) list

{-
Ground Truth Metadata:
Error Type: Tippfehler im Funktionsnamen / Typo in Function Name
Short Description: The function 'maps' is used instead of 'map', causing an undefined function error.
Intended Root Cause: Student introduces a typographical error in the function name.
Error Class: Mental Typo (Korkmaz et al. 2015, Level 0)
Affected Line(s): 2
-}
-- RegEx-Cluster: Funktion nicht definiert
