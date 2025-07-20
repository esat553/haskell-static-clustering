data Player = Player Name Team Shirt Foot Position deriving Show
type Name = String
type Team = String

type Shirt = Int
data Foot = LeftF | RightF deriving (Show, Eq)
data Position = Goalkeeper | Defender | Midfielder | Forward deriving (Show, Eq)
messi = Player "Messi" "Inter Miami CF" 10 Left

{-
Ground Truth Metadata:
Error Type: Arity-Fehler / Arity Mismatch
Short Description: The attribute for the 'Player' data type is missing so the value 'messi' is constructed with only four arguments instead of the required five.
Intended Root Cause: Student overlooks the required number of arguments for the data constructor and omits one during instantiation.
Error Class: Misconception (nach Korkmaz et al. 2015)
Affected Line(s): 8
-}
-- RegEx-Cluster: Falsche Funktionsarität
