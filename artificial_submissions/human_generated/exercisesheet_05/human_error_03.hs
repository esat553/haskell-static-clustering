data player = player Name Team Shirt Foot Position deriving Show
type name = String
type team = String

type shirt = Int
data foot = LeftF | RightF deriving (Show, Eq)
data position = Goalkeeper | Defender | Midfielder | Forward deriving (Show, Eq)
messi = player "Messi" "Inter Miami CF" 10 LeftF Forward

{-
Ground Truth Metadata:
Error Type: Falsche Groß-/Kleinschreibung bei Datentypen / Incorrect Capitalization of Data Types
Short Description: Data type and constructor names are written in lowercase ('player', 'name', etc.) instead of the required uppercase notation in Haskell.
Intended Root Cause: Student is unaware of or overlooks Haskell's convention that all data type and constructor names must start with a capital letter.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 1–7
-}
-- RegEx-Cluster: Fehler mit Datenkonstruktoren
