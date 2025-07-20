class Size a where
  size :: a -> Int

instance Size Bool where
  size True = 1
 size False = 1

{-
Ground Truth Metadata:
Error Type: Falsche Einrückung / Incorrect Indentation
Short Description: The second clause of the function `size` is not properly indented, violating Haskell’s layout rule and making the code syntactically invalid.
Intended Root Cause: Student does not adhere to Haskell’s whitespace-sensitive syntax and incorrectly assumes indentation is optional or stylistic.
Affected Line(s): 6
-}
-- RegEx-Cluster: Parse-Fehler