foo = sqrt . (5 *) . sum . flip [1..50]

{-
Ground Truth Metadata:
Error Type: Fehlerhafter Funktionsaufruf / Wrong Function Call Syntax
Short Description: The function 'take' is omitted in the composition, so 'flip [1..50]' is an invalid expression and does not apply the list to 'take' as intended.
Intended Root Cause: Student forgets to include the required function in the composition, resulting in an incomplete or incorrect function call.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Typenkonflikt