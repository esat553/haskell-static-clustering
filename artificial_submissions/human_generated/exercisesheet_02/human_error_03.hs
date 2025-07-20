rest :: String -> String
resti xs = drop 1 xs

{-
Ground Truth Metadata:
Error Type: Schreibfehler im Funktionsnamen / Spelling Error in Function Name
Short Description: The function 'resti' is defined instead of 'rest', causing a mismatch with the declared function name.
Intended Root Cause: Student makes a typographical error in the function name, leading to inconsistency between the signature and the implementation.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 2
-}
-- RegEx-Cluster: Fehlendes Binding