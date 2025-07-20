data Size a where
  size :: a -> Int

{-
Ground Truth Metadata:
Error Type: Verwechslung von 'class' und 'data' / Confusion Between 'class' and 'data'
Short Description: The keyword `data` is incorrectly used to define a type class, which must be declared using the keyword `class`.
Intended Root Cause: Student confuses the syntax for defining data types and type classes and uses `data` instead of `class` to introduce a type class.
Affected Line(s): 1
-}
-- RegEx-Cluster: Parse-Fehler