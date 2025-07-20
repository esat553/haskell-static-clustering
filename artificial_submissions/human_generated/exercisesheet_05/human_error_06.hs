data Player = Player Name Team Shirt Foot Position deriving Show
type Name = String
type Team = String

type Shirt = Int
data Foot = LeftF | RightF deriving (Show, Eq)
data Position = Goalkeeper | Defender | Midfielder | Forward deriving (Show, Eq)
messi = Player "Messi" "Inter Miami CF" 10 Left Forward

{-
Ground Truth Metadata:
Error Type: Verwendung undefinierter Bezeichner / Use of Undefined Identifier
Short Description: The identifier 'Left' is used instead of the correct constructor 'LeftF'; 'Left' is not defined in this context.
Intended Root Cause: Student confuses the correct constructor name and omits the suffix 'F', possibly mixing it up with other common names or standard library identifiers.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 8
-}
-- RegEx-Cluster: Falsche Funktionsarität