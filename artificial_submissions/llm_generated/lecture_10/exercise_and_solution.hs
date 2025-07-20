{-
Implementiere eine Funktion mergeConfigs :: [Config] -> Config, die eine Liste von Konfigurationsdateien zu einer einzigen zusammenführt.
Die Zusammenführung soll alle Listen konkatenieren und Metadaten-Strings mit Leerzeichen verbinden. Nutze die Monoid-Struktur von Config und implementiere die nötigen Instanzen für Semigroup und Monoid.
-}
{-
english translation:
Implement a function `mergeConfigs :: [Config] -> Config` that merges a list of configuration files into a single one.
The merging should concatenate all lists and join metadata strings with spaces. Use the `Monoid` structure of `Config` and implement the necessary instances for `Semigroup` and `Monoid`.
-}

-- precode
data Config = Config 
  { servers :: [String]
  , timeouts :: [Int] 
  , flags :: [Bool]
  , metadata :: String
  } deriving (Show, Eq)


-- solution
instance Semigroup Config where
  c1 <> c2 = Config
    { servers  = servers c1 <> servers c2
    , timeouts = timeouts c1 <> timeouts c2
    , flags    = flags c1 <> flags c2
    , metadata = case (metadata c1, metadata c2) of
        ("", s2) -> s2
        (s1, "") -> s1
        (s1, s2) -> s1 ++ " " ++ s2
    }

instance Monoid Config where
  mempty = Config [] [] [] ""

mergeConfigs :: [Config] -> Config
mergeConfigs = mconcat