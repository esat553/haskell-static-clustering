{-
Definiere eine eigene Typklasse Size mit einer Funktion size :: a -> Int, und implementiere jeweils Instanzen für Bool, Maybe a, Either a b und Listen. Die Größe soll die Anzahl der enthaltenen Werte zählen (bei Listen z. B. die Länge, bei Just oder Right jeweils 1).
-}
{-
english translation:
Define your own type class Size with a function size :: a -> Int, and implement instances for Bool, Maybe a, Either a b, and lists. The size should count the number of contained values (for lists, for example, the length; for Just or Right, 1).
-}

class Size a where
    size :: a -> Int

instance Size Bool where
    size _ = 1

instance Size (Maybe a) where
    size Nothing  = 0
    size (Just _) = 1

instance Size [a] where
    size = length

instance Size (Either a b) where
    size (Left _)  = 0
    size (Right _) = 1
