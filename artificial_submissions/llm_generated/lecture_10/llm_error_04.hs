-- precode
data Config = Config 
  { servers :: [String]
  , timeouts :: [Int] 
  , flags :: [Bool]
  , metadata :: String
  } deriving (Show, Eq)


instance Semigroup of Config where
  c1 <> c2 = Config
    { servers  = servers c1 <> servers c2
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
Error Type: Syntaxfehler bei Instanz-Deklaration / Instance Declaration Syntax Error
Short Description: The keyword `of` is used in the instance declaration, which is not valid Haskell syntax. The correct syntax is `instance Semigroup Config where`.
Intended Root Cause: Student is unfamiliar with the precise syntax for instance declarations and invents a plausible but incorrect structure, possibly by analogy with spoken language ('an instance of Semigroup for Config').
Affected Line(s): 10
-}
-- RegEx-Cluster: Parse-Fehler