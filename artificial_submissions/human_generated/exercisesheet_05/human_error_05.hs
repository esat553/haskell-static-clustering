data Player = Player Name Team Shirt Foot Position deriving Show
type Name = String
type Team = String

type Shirt = Int
data Foot = LeftF | RightF deriving (Show, Eq)
data Position = Goalkeeper | Defender | Midfielder | Forward deriving (Show, Eq)
messi = Player "Messi" 10 "Inter Miami CF" LeftF Forward

{-
Ground Truth Metadata:
Error Type: Falsche Argumentreihenfolge / Wrong Argument Order
Short Description: The arguments for the 'Player' constructor are provided in the wrong order; '10' (shirt number) and '"Inter Miami CF"' (team) are swapped.
Intended Root Cause: Student makes a careless swap of arguments while typing, despite knowing the correct order.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 8
-}
-- RegEx-Cluster: Typenkonflikt