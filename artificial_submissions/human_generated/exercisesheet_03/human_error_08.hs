take' :: Integer -> [a] -> [a]
take' z [] = []
take' z (x:xs)  | z <= 0 = []
                | otherwise = x : (take' (z - 1) xs)
-- tail-recursive

take'' :: Num -> [a] -> [a]
take'' z xs = tk z xs []
    where tk z [] acc = acc
          tk z (x:xs) acc   | z <= 0 = acc
                            | otherwise = tk (z - 1) xs (acc ++ [x])


{-
Ground Truth Metadata:
Error Type: Typklassenfehler in Typensignatur / Typeclass Error in Type Signature
Short Description: The type signature uses 'Num' as a concrete type in 'take''', but 'Num' is a type class, not a concrete type.
Intended Root Cause: Student confuses a type class with a type constructor, incorrectly using the type class name in a type signature.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 7
-}
-- RegEx-Cluster: Falsche Anzahl von Typ-Argumenten