class Size a
    size :: a -> Int

instance Size [a]
    size(xs) = length(xs)

instance Size Bool
    size(b) = 1

{-
Ground Truth Metadata:
Error Type: Falsche Klassendefinition und Funktionssyntax / Incorrect Class Definition and Function Syntax
Short Description: The class declaration is missing the `where` keyword, and function definitions incorrectly use parentheses around parameters, resembling C-style syntax rather than Haskell's pattern matching.
Intended Root Cause: Student is unfamiliar with Haskell's syntax for type classes and function declarations, leading to a mix of imperative and functional notation.
Affected Line(s): 1–6
-}
-- RegEx-Cluster: Parse-Fehler