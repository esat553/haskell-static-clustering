data Player = Player Name Team Shirt Foot Position deriving Show
type Name = String
type Team = String

type Shirt = Int
data Foot = LeftF | RightF deriving (Show, Eq)
data Position = Goalkeeper | Defender | Midfielder | Forward deriving (Show, Eq)
messi = Player 'Messi' "Inter Miami CF" 10 LeftF Forward


{-
Ground Truth Metadata:
Error Type: Falsche Zeichenart / Wrong Quotation Type
Short Description: Single quotes are used for 'Messi', which denotes a character literal in Haskell. Double quotes are required for string literals.
Intended Root Cause: Student confuses the notation for strings and characters, using single quotes for a multi-character string.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 8
-}
-- RegEx-Cluster: Ungültiges Zeichen