data Foo = Bar deriving (show)

{-
Studentenlösung:1:26-29: error:
    • Illegal deriving item ‘show’
    • In the data declaration for ‘Foo’
  |
1 | data Foo = Bar deriving (show)
  |                          ^^^^
-}