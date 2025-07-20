data Player = Player Name Team Shirt Foot Position deriving Show
type Name = String
type Team = String

type Shirt = Int
data Foot = LeftF | RightF deriving (Show, Eq)
data Position = Goalkeeper | Defender | Midfielder | Forward deriving (Show, Eq)
messi = Playr "Messi" "Inter Miami CF" 10 LeftF Forward

{-
Ground Truth Metadata:
Error Type: Schreibfehler im Konstruktor / Spelling Error in Constructor Name
Short Description: The constructor 'Playr' is misspelled instead of the correct 'Player' when constructing the value 'messi'.
Intended Root Cause: Student makes a typographical error in the constructor name when instantiating the data type.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 8
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich