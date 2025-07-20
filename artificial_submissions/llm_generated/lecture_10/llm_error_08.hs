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
  mempty = Config [] [] [] ""

mergeConfigs :: [Config] -> Config
mergeConfigs configs = foldr (<>) configs

{-
Ground Truth Metadata:
Error Type: Typfehler bei `foldr`-Anwendung / Type Error in `foldr` Application
Short Description: The `foldr` function is called with an initial value (`configs`, type `[Config]`) that does not match the function's expected accumulator type (`Config`).
Intended Root Cause: Student correctly identifies `foldr` as a way to reduce a list but incorrectly provides the list itself as the starting accumulator instead of the correct neutral element for the operation, `mempty`.
Affected Line(s): 22
-}
-- RegEx-Cluster: Falsche Funktionsarität