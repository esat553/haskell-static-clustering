class Size a where
  size :: a -> Int

instance Size [a] where
  size [] = 0
  size (x:xs) = x + size xs

{-
Ground Truth Metadata:
Error Type: Typfehler durch falsche Annahme über Elementtyp / Type Error Due to Incorrect Assumption About Element Type
Short Description: The expression `x + size xs` assumes that list elements are numeric (`Int`), but `x` has type `a` and no such constraint is given.
Intended Root Cause: Student incorrectly assumes that elements of the list are integers and uses arithmetic operations without restricting the type variable.
Affected Line(s): 6
-}
-- Reg-Ex-Clustering: Implementierung verletzt Typsignatur