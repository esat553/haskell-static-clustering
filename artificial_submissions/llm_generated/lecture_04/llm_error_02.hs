dropLastN :: Int -> [a] -> [a]
dropLastN n []
    | n > 0 = "Liste ist zu kurz"
    | otherwise = []
dropLastN n (x:xs) = x : dropLastN n xs
{-
Ground Truth Metadata:
Error Type: Inkonsistenter Rückgabetyp / Inconsistent Return Type
Short Description: In one branch of the function, a `String` is returned instead of a list, which contradicts the declared return type `[a]`.
Intended Root Cause: Student attempts to handle an error case with a user-friendly message but does not align the return type with the function signature.
Affected Line(s): 3
-}
-- RegEx-Cluster: Implementierung verletzt Typsignatur