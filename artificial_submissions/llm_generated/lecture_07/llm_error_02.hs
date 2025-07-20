class Size a where
  size :: a -> Int

instance Size Maybe where
  size (Just _) = 1
  size Nothing  = 0

instance Size Either where
  size (Left _)  = 1
  size (Right _) = 1

{-
Ground Truth Metadata:
Error Type: Fehlende Typvariablen in Instanzdefinition / Missing Type Variables in Instance Declaration
Short Description: The instance declarations for `Maybe` and `Either` omit required type variables, resulting in malformed instance heads.
Intended Root Cause: Student assumes that type constructors alone are sufficient in instance declarations and overlooks that full type application (e.g. `Maybe a`, `Either a b`) is required.
Affected Line(s): 4, 8
-}
-- RegEx-Cluster: Falsche Anzahl von Typ-Argumenten