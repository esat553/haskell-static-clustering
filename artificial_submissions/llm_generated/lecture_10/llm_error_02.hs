-- precode
data Config = Config 
  { servers :: [String]
  , timeouts :: [Int] 
  , flags :: [Bool]
  , metadata :: String
  } deriving (Show, Eq)


instance Semigroup Config where
  c1 <> c2 = Config
    { servers  = servers c1 + servers c2
    , timeouts = timeouts c1 + timeouts c2
    , flags    = flags c1 <> flags c2
    , metadata = metadata c1 ++ " " ++ metadata c2
    }

instance Monoid Config where
  mempty = Config [] [] [] ""

mergeConfigs :: [Config] -> Config
mergeConfigs = mconcat

{-
Ground Truth Metadata:
Error Type: Typfehler bei Listenkonkatenation / Type Error in List Concatenation
Short Description: The `+` operator is used to combine lists, but it is defined for numeric types (in the `Num` typeclass), not for lists. The correct operator is `++` or `<>`.
Intended Root Cause: Student mistakenly applies an arithmetic operator to lists, likely confusing it with the list concatenation operator (`++`) or with similar operators in other languages (e.g., Python, JavaScript) where `+` can concatenate sequences.
Affected Line(s): 12, 13
-}
-- RegEx-Cluster: Numerischer Typenkonflikt