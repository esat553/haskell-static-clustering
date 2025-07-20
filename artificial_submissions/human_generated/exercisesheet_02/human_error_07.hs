rest' : String -> String
rest' (_:xs) = xs

{-
Ground Truth Metadata:
Error Type: Falsche Syntax in Typsignatur / Wrong Syntax in Type Signature
Short Description: The type signature uses a single colon ':' instead of the required double colon '::'.
Intended Root Cause: Student confuses the correct syntax for type signatures, writing ':' instead of '::'.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Parse-Fehler