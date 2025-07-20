-- precode
data Config = Config 
  { servers :: [String]
  , timeouts :: [Int] 
  , flags :: [Bool]
  , metadata :: String
  } deriving (Show, Eq)


instance Monoid Config where
  mempty = Config [] [] [] ""
  mappend c1 c2 = Config
    { servers  = servers c1 ++ servers c2
    , timeouts = timeouts c1 ++ timeouts c2
    , flags    = flags c1 ++ flags c2
    , metadata = metadata c1 ++ " " ++ metadata c2
    }

mergeConfigs :: [Config] -> Config
mergeConfigs = mconcat

{-
Ground Truth Metadata:
Error Type: Fehlende Superclass-Instanz / Missing Superclass Instance
Short Description: An instance for `Monoid Config` is defined without a corresponding `Semigroup Config` instance, which is a required superclass for `Monoid` in modern GHC versions.
Intended Root Cause: Student is unaware of the superclass relationship `instance Semigroup a => Monoid a` and the constraint it imposes, possibly due to learning from outdated materials or misunderstanding typeclass dependencies.
Affected Line(s): /
-}
-- RegEx-Cluster: Fehlende Superklassen-Instanz