data Player = Player Name Team Shirt Foot Position deriving Showing
type Name = String
type Team = String

type Shirt = Int
data Foot = LeftF | RightF deriving (Show, Eq)
data Position = Goalkeeper | Defender | Midfielder | Forward deriving (Show, Eq)
messi = Player "Messi" "Inter Miami CF" 10 LeftF Forward


{-
Ground Truth Metadata:
Error Type: Falscher Name für Typklassenableitung / Wrong Name for Typeclass Derivation
Short Description: The keyword 'Showing' is used in the 'deriving' clause instead of the correct type class name 'Show'.
Intended Root Cause: Student confuses or mistypes the name of the standard type class to derive, using 'Showing' instead of 'Show'.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Typenkonstruktor oder Klasse nicht definiert