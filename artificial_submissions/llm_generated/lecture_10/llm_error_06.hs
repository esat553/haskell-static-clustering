-- precode
data Config = Config 
  { servers :: [String]
  , timeouts :: [Int] 
  , flags :: [Bool]
  , metadata :: String
  } deriving (Show, Eq)

instance Semigroup Config where
  c1 <> c2 = Config
    { servers  = servers <> servers
    , timeouts = timeouts c1 <> timeouts c2
    , flags    = flags c1 <> flags c2
    , metadata = metadata c1 ++ " " ++ metadata c2
    }

instance Monoid Config where
  mempty = Config [] [] [] ""

mergeConfigs :: [Config] -> Config
mergeConfigs = mconcat

{-
Ground Truth Metadata:
Error Type: Fehler beim Zugriff auf Record-Felder / Record Field Access Error
Short Description: The code attempts to use the record accessor function `servers` as if it were a value, instead of applying it to an instance of the record (e.g., `servers c1`).
Intended Root Cause: Student misunderstands that record field names also act as accessor functions that need to be applied to a record value, and instead treats them as if they are implicitly scoped local variables.
Affected Line(s): 11
-}
-- RegEx-Cluster: Falsche Funktionsarität