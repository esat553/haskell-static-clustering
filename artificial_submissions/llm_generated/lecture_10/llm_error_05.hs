-- precode
data Config = Config 
  { servers :: [String]
  , timeouts :: [Int] 
  , flags :: [Bool]
  , metadata :: String
  } deriving (Show, Eq)

instance Semigroup Config where
  mappend c1 c2 = Config
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
Error Type: Falsche Methodenimplementierung in Typklasse / Incorrect Typeclass Method Implementation
Short Description: The implementation provides a definition for `mappend` within the `Semigroup` instance, but `Semigroup` requires a minimal implementation for the `<>` operator.
Intended Root Cause: Student confuses the methods of `Semigroup` (`<>`) and `Monoid` (`mappend`), possibly due to knowledge of older GHC versions where `mappend` was the primary function before the Semigroup-Monoid-Proposal.
Affected Line(s): 10
-}
-- RegEx-Cluster: Methode nicht in Klasse