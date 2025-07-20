-- recursive
take' z [] = []
take' z (x:xs)  | z <= 0 = []
                | otherwise = x : (take' (z - 1) xs)
-- tail-recursive
take'' z xs = tk z xs []
    where tk z [] acc = acc
          tk z (x:xs) acc   | z <= 0 = acc
                            | otherwise = tk (z - 1) (acc ++ x) xs


{-
Ground Truth Metadata:
Error Type: Falsche Reihenfolge der Funktionsargumente / Wrong Argument Order in Function Call
Short Description: The arguments in the recursive call 'tk (z - 1) (acc ++ x) xs' are in the wrong order; 'acc ++ [x]' and correct placement of 'xs' are expected.
Intended Root Cause: Student confuses the expected order of arguments when making a recursive call, leading to an incorrect function invocation.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 9
-}
-- RegEx-Cluster: Implementierung verletzt Typsignatur