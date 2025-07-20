analyse :: [String] -> [(Char, Int)]
analyse xs = 
    let firstChars = map head xs
    in map (\s -> 
        let c = head s
            count = foldr (\x acc => if x == c then acc + 1 else acc) 0 firstChars
        in (c, count)
    ) xs

{-
Ground Truth Metadata:
Error Type: Falsche Syntax in Lambda-Ausdruck / Incorrect Syntax in Lambda Expression
Short Description: The lambda function inside `foldr` uses `=>` instead of `->`, which is invalid syntax in Haskell for defining anonymous functions.
Intended Root Cause: Student confuses the arrow used in type constraints (`=>`) with the arrow used in lambda expressions (`->`), leading to a syntax error.
Affected Line(s): 6
-}
-- RegEx-Cluster: Parse-Fehler