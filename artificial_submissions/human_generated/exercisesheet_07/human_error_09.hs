foo :: Int -> Double
foo a = sqrt . (5 *) . sum . flip take [1..50]

{-
Ground Truth Metadata:
Error Type: Nicht verwendeter Funktionsparameter / Unused Function Parameter
Short Description: The parameter 'a' is declared in the function definition, but it is not used in the function body.
Intended Root Cause: Student misunderstands how to pass parameters in pointfree or higher-order function definitions, leading to the introduction of unnecessary or unused parameters.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 2
-}
-- RegEx-Cluster: Falsche Funktionsarität