data TestTypA = WertA1 | WertA2
data TestTypB = WertB1 | WertB2

data TestStruktur = TestStruktur
  { feldA :: TestTypA
  , feldB :: TestTypB
  }

istWertA1 :: TestStruktur -> Bool
istWertA1 s = feldA s == WertA1

istWertB1 :: TestStruktur -> Bool
istWertB1 s = feldB s == WertB1
{-
error:
    • No instance for (Eq TestTypA) arising from a use of ‘==’
    • In the expression: feldA s == WertA1
      In an equation for ‘istWertA1’: istWertA1 s = feldA s == WertA1
   |
10 | istWertA1 s = feldA s == WertA1
   |                       ^^

error:
    • No instance for (Eq TestTypB) arising from a use of ‘==’
    • In the expression: feldB s == WertB1
      In an equation for ‘istWertB1’: istWertB1 s = feldB s == WertB1
   |
13 | istWertB1 s = feldB s == WertB1
   |                       ^^
-}