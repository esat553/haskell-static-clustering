data Player = Player Name Team Shirt Foot Position deriving Show
type Name = String
type Team = String

type Shirt = Integral
data Foot = LeftF | RightF deriving (Show, Eq)
data Position = Goalkeeper | Defender | Midfielder | Forward deriving (Show, Eq)
messi = Player "Messi" "Inter Miami CF" 10 LeftF Forward

{-
Ground Truth Metadata:
Error Type: Typklassenfehler in Typensignatur / Typeclass Error in Type Signature
Short Description: The type 'Integral' is used for 'Shirt', but 'Integral' is a type class, not a concrete type. The correct type would be 'Integer' or 'Int'.
Intended Root Cause: Student confuses a type class with a concrete data type and uses the type class name in a type declaration.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 5
-}
-- RegEx-Cluster: Falsche Anzahl von Typ-Argumenten