data Foo = Foo Int deriving (Enum)

{-
error:
    • Can't make a derived instance of ‘Enum Foo’:
        ‘Foo’ must be an enumeration type
        (an enumeration consists of one or more nullary, non-GADT constructors)
    • In the data declaration for ‘Foo’
  |
1 | data Foo = Foo Int deriving (Enum)
  |                              ^^^^
-}