analyse :: [String] -> [(Char, Int)]
analyse xs =
    let firstChars = map head xs
    in map (\s -> 
        let c = head s
            matchingChars = filter (firstChars == [c]) firstChars
        in (c, length matchingChars)
    ) xs

{-
Ground Truth Metadata:
Error Type: Falscher Argumenttyp für `filter` / Incorrect Argument Type for `filter`
Short Description: The expression `firstChars == [c]` is a `Bool`, but `filter` expects a predicate function of type `Char -> Bool`.
Intended Root Cause: Student confuses the use of `filter` with a boolean condition and mistakenly supplies a boolean expression instead of a function.
Affected Line(s): 6
-}
-- RegEx-Cluster: Falsche Funktionsarität