data Suit = Red | Blue | Green | Yellow
            deriving (Eq, Ord)
data Value = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
            deriving (Eq, Ord)
data Card = Value `Of` Suit
            deriving (Eq, Show)

{-
error:
    • No instance for (Show Value)
        arising from the first field of ‘Of’ (type ‘Value’)
      Possible fix:
        use a standalone 'deriving instance' declaration,
          so you can specify the instance context yourself
    • When deriving the instance for (Show Card)
  |
6 |             deriving (Eq, Show)
  |                           ^^^^
-}