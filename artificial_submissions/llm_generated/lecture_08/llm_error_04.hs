analyse :: [String] -> [(Char, Int)]
analyse xs = 
    map (\s -> (head s, count)) xs
  where
    firstChars = map head xs
    count = foldr (\c acc -> if c == (head s) then acc + 1 else acc) 0 firstChars

{-
Ground Truth Metadata:
Error Type: Ungültiger Variablenzugriff außerhalb des Gültigkeitsbereichs / Invalid Variable Access Outside Scope
Short Description: The variable `s` is used in the definition of `count`, but `s` is only bound within the lambda function inside `map` and is not accessible in the `where` block.
Intended Root Cause: Student misunderstands lexical scoping and attempts to use a locally bound variable (`s`) in a global definition, assuming it is accessible outside its defining context.
Affected Line(s): 6
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich