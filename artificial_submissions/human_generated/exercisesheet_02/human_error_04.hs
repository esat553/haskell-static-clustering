rest :: String -> String
rest (x:xs) = drop x xs

{-
Ground Truth Metadata:
Error Type: Falscher Argumenttyp bei Funktionsaufruf / Wrong Argument Type in Function Call
Short Description: The 'drop' function is called with 'x' (a character) as its first argument, but it expects an integer specifying the number of elements to drop.
Intended Root Cause: Student confuses the expected argument type of the function, using a value of the wrong type in the call.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 2
-}
-- RegEx-Cluster: Typenkonflikt