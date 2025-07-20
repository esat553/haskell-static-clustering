Data Player = Player Name Team Shirt Foot Position deriving Show
Type Name = String
Type Team = String

Type Shirt = Int
Data Foot = LeftF | RightF deriving (Show, Eq)
Data Position = Goalkeeper | Defender | Midfielder | Forward deriving (Show, Eq)
messi = Player "Messi" "Inter Miami CF" 10 LeftF Forward

{-
Ground Truth Metadata:
Error Type: Falsche Groß-/Kleinschreibung in Typdefinition / Incorrect Capitalization in Type Declarations
Short Description: The keywords 'Data' and 'Type' are capitalized instead of being lowercase ('data' and 'type'), which is not valid Haskell syntax.
Intended Root Cause: Student misunderstands or overlooks Haskell's syntax rules for reserved keywords, using uppercase instead of the required lowercase spelling.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 1–7
-}
-- RegEx-Cluster: Parse-Fehler