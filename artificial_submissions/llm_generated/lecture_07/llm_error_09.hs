class Size a =>
  size :: a -> Int

instance Size a -> Size (Maybe a) where
  size (Just _) = 1
  size Nothing  = 0

{-
Ground Truth Metadata:
Error Type: Falsche Verwendung von Pfeilsymbolen / Incorrect Use of Arrow Symbols
Short Description: The class declaration and instance definition use `=>` and `->` incorrectly—`where` is missing in the class header, and `->` is wrongly used instead of `=>` in the instance constraint.
Intended Root Cause: Student confuses the different arrow symbols in Haskell (`->`, `=>`) and misuses them in places that require specific keywords or type class syntax.
Affected Line(s): 1, 4
-}
-- RegEx-Cluster: Fehlerhafter Typ-Header