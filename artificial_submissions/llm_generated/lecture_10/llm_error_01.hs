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
      timeouts = timeouts c1 <> timeouts c2
      flags    = flags c1 <> flags c2
      metadata = metadata c1 ++ " " ++ metadata c2
    }

instance Monoid Config where
  mempty = Config [] [] [] ""

mergeConfigs :: [Config] -> Config
mergeConfigs = mconcat

{-
Ground Truth Metadata:
Error Type: Syntaxfehler bei Record-Erstellung / Record Syntax Error
Short Description: The code omits the required comma to separate field initializers within the record construction syntax between the 'servers' and 'timeouts' fields.
Intended Root Cause: Student forgets the comma separator required between fields in a record initialization, possibly due to a simple typo or being accustomed to languages with different syntaxes for object/struct literals.
Affected Line(s): 11-14
-}
-- RegEx-Cluster: Parse-Fehler