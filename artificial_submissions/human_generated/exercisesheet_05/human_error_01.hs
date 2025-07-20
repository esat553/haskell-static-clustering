data Player = Player Name Team Shirt Foot Position deriving Show, Eq
type Name = String
type Team = String

type Shirt = Int
data Foot = LeftF | RightF deriving (Show, Eq)
data Position = Goalkeeper | Defender | Midfielder | Forward deriving (Show, Eq)
messi = Player "Messi" "Inter Miami CF" 10 LeftF Forward

{-
Ground Truth Metadata:
Error Type: Fehlerhafte deriving-Syntax / Incorrect deriving Syntax
Short Description: Multiple type classes ('Show', 'Eq') are listed in the 'deriving' clause without parentheses; in Haskell, multiple derivings must be enclosed in parentheses.
Intended Root Cause: Student is unfamiliar with the correct syntax for deriving multiple type classes and omits the required parentheses.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Parse-Fehler