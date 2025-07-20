data box a = full a | empty

instance Functor box where
  fmap f (full x) = full (f x)
  fmap _ empty    = empty

{-
Ground Truth Metadata:
Error Type: Ungültige Groß-/Kleinschreibung bei Typ- und Datenkonstruktoren / Invalid Capitalization of Type and Data Constructors
Short Description: The type constructor `box` and the data constructors `full` and `empty` are written in lowercase, but Haskell requires type and data constructors to begin with uppercase letters.
Intended Root Cause: Student overlooks Haskell's naming conventions and uses lowercase identifiers for types and constructors, which are reserved for variables.
Affected Line(s): 1, 3
-}
-- RegEx-Cluster: Fehler mit Datenkonstruktoren