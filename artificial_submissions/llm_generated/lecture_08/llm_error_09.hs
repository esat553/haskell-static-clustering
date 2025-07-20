analyse :: [String] -> [(Char, Int)]
analyse xs = 
    map (\s -> (head s, count s)) xs
  where
    zählBuchstaben c chars = foldr (\x acc -> if x == c then acc + 1 else acc) 0 chars
    count c = zählBuchstaben c

{-
Ground Truth Metadata:
Error Type: Unvollständige Funktionsanwendung / Incomplete Function Application
Short Description: The function `count` calls `zählBuchstaben` with only one argument, although two are required; the second argument is missing entirely.
Intended Root Cause: Student forgets to pass the list of characters to the helper function and assumes partial application is sufficient for full evaluation.
Affected Line(s): 7
-}
-- RegEx-Cluster: Falsche Funktionsarität