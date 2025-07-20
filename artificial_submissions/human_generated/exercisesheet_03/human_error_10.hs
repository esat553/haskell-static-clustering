take' z empty = empty
take' z (x:xs)  | z <= 0 = empty
                | otherwise = x : (take' (z - 1) xs)


{-
Ground Truth Metadata:
Error Type: Verwendung undefinierter Bezeichner / Use of Undefined Identifier
Short Description: The identifier 'empty' is used to represent the empty list, but 'empty' is not defined; the correct value is '[]'.
Intended Root Cause: Student confuses the standard representation of an empty list in Haskell and uses 'empty' instead of '[]'.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 1, 2
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich