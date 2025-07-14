data Suit = Club | Diamond | Heart | Spade
  deriving (Eq)

instance Eq Suit where
  Club == Club = True
  _ == _       = False

  {-
  error:
    Duplicate instance declarations:
      instance Eq Suit -- Defined at Studentenlösung:2:13
      instance Eq Suit -- Defined at Studentenlösung:4:10
  |
2 |   deriving (Eq)
  |             ^^
  -}