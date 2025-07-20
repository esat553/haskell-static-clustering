take' :: Integer -> [String] -> String
take' z [] = []
take' z (x:xs)  | z <= 0 = []
                | otherwise = x : (take' (z - 1) xs)
-- tail-recursive

take'' :: Integer -> [a] -> [a]
take'' z xs = tk z xs []
    where tk z [] acc = acc
          tk z (x:xs) acc   | z <= 0 = acc
                            | otherwise = tk (z - 1) xs (acc ++ [x])


{-
Ground Truth Metadata:
Error Type: Typeninkonsistenz / Type Inconsistency
Short Description: The type signature for 'take'' expects a result of type 'String', but the function is written to return a list of strings ('[String]').
Intended Root Cause: Student confuses the expected output type and incorrectly declares the result as 'String' instead of '[String]'.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Typenkonflikt