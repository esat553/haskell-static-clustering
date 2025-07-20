-- precode
data Config = Config 
  { servers :: [String]
  , timeouts :: [Int] 
  , flags :: [Bool]
  , metadata :: String
  } deriving (Show, Eq)

-- Falsch: Syntaxfehler im case-Ausdruck
instance Semigroup Config where
  c1 <> c2 = Config
    { servers  = servers c1 <> servers c2
    , timeouts = timeouts c1 <> timeouts c2
    , flags    = flags c1 <> flags c2
    , metadata = case (metadata c1, metadata c2) of
        ("", s2) s2  
        (s1, "") s1  
        (s1, s2) s1 ++ " " ++ s2
    }

instance Monoid Config where
  mempty = Config [] [] [] ""

mergeConfigs :: [Config] -> Config
mergeConfigs = mconcat

{-
Ground Truth Metadata:
Error Type: Syntaxfehler im `case`-Ausdruck / Case Expression Syntax Error
Short Description: The `case` expression is syntactically incorrect because it is missing the `->` arrow required to separate the patterns from their corresponding expressions.
Intended Root Cause: A simple typographical error or a student who is not yet familiar with the mandatory `->` syntax in `case` expressions, perhaps expecting separation by whitespace alone.
Affected Line(s): 16, 17, 18
-}
-- RegEx-Cluster: Parse-Fehler