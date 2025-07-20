foo = sqrt . (5 *) . sum . take [1..50] flip

{-
Ground Truth Metadata:
Error Type: Fehlerhafter Funktionsaufruf / Wrong Function Call Syntax
Short Description: The function 'flip' is applied incorrectly after 'take [1..50]', resulting in an invalid function call and incorrect argument order.
Intended Root Cause: Student misunderstands the usage and placement of 'flip' in function composition, leading to a syntactically incorrect or unintended application.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Falsche Funktionsarität