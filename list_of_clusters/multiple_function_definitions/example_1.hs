foo a a = a

{-
error:
    • Conflicting definitions for ‘a’
      Bound at: Studentenlösung:1:5
                Studentenlösung:1:7
    • In an equation for ‘foo’
  |
1 | foo a a = a
  |     ^^^
-}