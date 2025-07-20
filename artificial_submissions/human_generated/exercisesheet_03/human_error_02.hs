take' z [] = []
take' z (x,xs)  | z <= 0 = []
                | otherwise = x : (take' (z - 1) xs)


{-
Ground Truth Metadata:
Error Type: Fehlerhaftes Pattern Matching / Incorrect Pattern Matching
Short Description: A comma ',' is used instead of a colon ':' in the pattern matching of the list, leading to an incorrect pattern.
Intended Root Cause: Student confuses the syntax for deconstructing lists, using a comma where a colon is required.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 2
-}
-- RegEx-Cluster: Typenkonflikt