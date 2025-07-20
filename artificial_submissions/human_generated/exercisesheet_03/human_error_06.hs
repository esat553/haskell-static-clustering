-- recursive
take' z [] = []
take' z (x:xs)  | z <= 0 = []
                | otherwise = x : (take' (z - 1) xs)
-- tail-recursive
take'' z xs = tk z xs []
    where tk z [] acc = acc
          tk z (x:xs) acc   | z <= 0 = acc
                            | otherwise = tk 4 (z - 1) xs (acc ++ [x])

{-
Ground Truth Metadata:
Error Type: Arity-Fehler / Arity Mismatch
Short Description: The recursive call 'tk 4 (z - 1) xs (acc ++ [x])' uses four arguments instead of the expected three for the function definition.
Intended Root Cause: Student misunderstands the function's parameter structure and supplies too many arguments in the recursive call.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 9
-}
-- RegEx-Cluster: Typenkonflikt