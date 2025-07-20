wertetabelle_f8 :: Integer -> Integer -> (Integer, String)
wertetabelle_f8 a b = 
    [ (x, "++g1") | x <- [a..b] ]

{-
Ground Truth Metadata:
Error Type: Typ-Signatur passt nicht zur Implementierung / Type Signature Does Not Match Implementation
Short Description: The type signature declares a single tuple `(Integer, String)` as the return type, but the implementation is a list comprehension that produces a list of such tuples, i.e., `[(Integer, String)]`.
Intended Root Cause: Student overlooks the distinction between a single value and a list of values in the type signature, especially in the context of list comprehensions.
Affected Line(s): 1, 3
-}
-- RegEx-Cluster: Typenkonflikt