data Foo a a = Bar a a

{-
error:
    Conflicting definitions for ‘a’
    Bound at: Studentenlösung:1:10
              Studentenlösung:1:12
  |
1 | data Foo a a = Bar a a
  |          ^^^
-}