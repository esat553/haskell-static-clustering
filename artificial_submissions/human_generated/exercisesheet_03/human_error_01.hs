take' z (x:xs)  = z <= 0 = []
                | otherwise = x : (take' (z - 1) xs)


{-
Ground Truth Metadata:
Error Type: Fehlerhafte Guard-Syntax / Incorrect Guard Syntax
Short Description: The equals sign '=' is used instead of the required guard symbol '|' in the function definition, leading to incorrect guard syntax.
Intended Root Cause: Student lacks knowledge of the correct syntax for guards in Haskell and confuses '=' with '|'.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Parse-Fehler