-- precode
data Config = Config 
  { servers :: [String]
  , timeouts :: [Int] 
  , flags :: [Bool]
  , metadata :: String
  } deriving (Show, Eq)


instance Semigroup Config where
  c1 <> c2 = Config
    { servers  = servers c1 <> servers c2
    , timeouts = timeouts c1 <> timeouts c2
    , flags    = flags c1 <> flags c2
    , metadata = metadata c1 ++ " " ++ metadata c2
    }

instance Monoid Config where
  mempty = Config [] [] []

mergeConfigs :: [Config] -> Config
mergeConfigs = mconcat

{-
Ground Truth Metadata:
Error Type: Fehler bei Konstruktor-Anwendung (Arity) / Incorrect Constructor Arity
Short Description: The `Config` data constructor is called with three arguments, but its definition requires four (for `servers`, `timeouts`, `flags`, and `metadata`).
Intended Root Cause: Student overlooks one of the fields of the data type when defining the neutral element (`mempty`) for the `Monoid` instance, leading to a mismatched number of arguments for the constructor.
Affected Line(s): 19
-}
-- RegEx-Cluster: Falsche Funktionsarität